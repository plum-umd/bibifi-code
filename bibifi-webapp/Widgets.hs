module Widgets where

import Control.Monad

import Import
import PostDependencyType

import qualified Database.Esqueleto as E

buildSubmission :: Entity BuildSubmission -> ContestId -> Bool -> LWidget
buildSubmission (Entity bsId bs) cId public = do
    let status = prettyBuildStatus $ buildSubmissionStatus bs
    time <- lLift $ liftIO $ displayTime $ buildSubmissionTimestamp bs
    lLift $ toWidget [lucius|
        .message-column {
            border-top-color: transparent !important;
            padding: 0px !important;
        }
         .message-column pre {
            margin: 8px;
         }
         .details-link {
            margin-left: 6px;
         }
    |]
    let judgementW :: LWidget = do
        judgementM <- handlerToWidget $ runDB $ getBy $ UniqueBuildJudgement bsId
        case judgementM of
            Nothing ->
                mempty
            Just (Entity jId j) -> 
                let ruling = case buildJudgementRuling j of
                      Nothing ->
                        [shamlet|
                            <span>
                                Pending
                        |]
                      Just True ->
                        [shamlet|
                            <span class="text-success">
                                Passed
                        |]
                      Just False ->
                        [shamlet|
                            <span class="text-danger">
                                Failed
                        |]
                in
                let comments = case buildJudgementComments j of
                      Nothing ->
                        dash
                      Just c ->
                        toHtml c
                in
                do
                [whamlet|
                    <div class="form-group">
                        <label class="col-xs-3 control-label">
                            Judgment
                        <div class="col-xs-9">
                            <p class="form-control-static">
                                #{ruling}
                |]
                when (not public) $
                    [whamlet|
                        <div class="form-group">
                            <label class="col-xs-3 control-label">
                                Judge comments
                            <div class="col-xs-9">
                                <p class="form-control-static">
                                    #{comments}
                    |]
    [whamlet|
        <form class="form-horizontal">
            <div class="form-group">
                <label class="col-xs-3 control-label">
                    Submission hash
                <div class="col-xs-9">
                    <p class="form-control-static">
                        #{buildSubmissionCommitHash bs}
            <div class="form-group">
                <label class="col-xs-3 control-label">
                    Timestamp
                <div class="col-xs-9">
                    <p class="form-control-static">
                        #{time}
            <div class="form-group">
                <label class="col-xs-3 control-label">
                    Status
                <div class="col-xs-9">
                    <p class="form-control-static">
                        #{status}
            ^{(judgementW)}
    |]
    when (not public) $ do
        case buildSubmissionStdout bs of
            Just stdout -> 
                [whamlet|
                    <h4>
                        Build Standard Output
                    <samp>
                        #{stdout}
                |]
            _ ->
                mempty
        case buildSubmissionStderr bs of
            Just stderr -> 
                [whamlet|
                    <h4>
                        Build Standard Error
                    <samp>
                        #{stderr}
                |]
            _ ->
                mempty
    if (buildSubmissionStatus bs) == BuildBuilt then
        let renderCores ((Entity _ test), mbr') = do
              rowId <- newIdent
              let ( result, message, collapse) = case mbr' of 
                    Nothing ->
                        ( prettyPassResult False, mempty, mempty)
                    Just (Entity _ mbr) ->
                        let res = prettyPassResult $ buildCoreResultPass mbr in
                        let (out, collapse) = testMessage rowId public $ buildCoreResultMessage mbr in
                        ( res, out, collapse)
              [whamlet'|
                  <tr>
                      <td>
                          #{contestCoreTestName test}
                      <td>
                          Correctness
                      <td>
                          #{result} #{collapse}
                      <td>
                          #{dash}
              |]
              message
        in
        let renderOpts ((Entity _ test), mbr') = do
              rowId <- newIdent
              let (result, message, collapse) = case mbr' of 
                    Nothing ->
                        (prettyPassResult False, mempty, mempty)
                    Just (Entity _ mbr) ->
                        let res = prettyPassResult $ buildOptionalResultPass mbr in
                        let (out, collapse) = testMessage rowId public $ buildOptionalResultMessage mbr in
                        ( res, out, collapse)
              [whamlet'|
                  <tr>
                      <td>
                          #{contestOptionalTestName test}
                      <td>
                          Optional
                      <td>
                          #{result} #{collapse}
                      <td>
                          #{dash}
              |]
              when (not public) $
                message
        in
        let renderPerfs ((Entity _ test), mbr') = do
              rowId <- newIdent
              let (result, period, message, collapse) = case mbr' of
                    Nothing ->
                        (prettyPassResult False, dash, mempty, mempty)
                    Just (Entity _ mbr) ->
                        case buildPerformanceResultTime mbr of
                            Nothing ->
                                let (out, collapse) = testMessage rowId public $ buildPerformanceResultMessage mbr in
                                ( prettyPassResult False, dash, out, collapse)
                            Just t ->
                                let period' = [shamlet|#{t}|] in
                                ( prettyPassResult True, period', mempty, mempty)
              let testType = 
                    if contestPerformanceTestOptional test then
                        "Performance" :: String
                    else
                        "Performance*"
              [whamlet'|
                  <tr>
                      <td>
                          #{contestPerformanceTestName test}
                      <td>
                          #{testType}
                      <td>
                          #{result} #{collapse}
                      <td>
                          #{period}
              |]
              message
        in
        do
        coreResults <- handlerToWidget $ runDB 
            -- [lsql| 
            --     select ContestCoreTest.*, BuildCoreResult.* from ContestCoreTest 
            --     left outer join BuildCoreResult on ContestCoreTest.id == BuildCoreResult.test
            --     where ContestCoreTest.contest == #{cId}
            --         and (BuildCoreResult.submission == #{Just bsId} or BuildCoreResult.submission is null)
            --     order by ContestCoreTest.name asc
            -- |]
            $ E.select $ E.from $ \(t `E.LeftOuterJoin` tr) -> do
                E.on ( E.just (t E.^. ContestCoreTestId) E.==. tr E.?. BuildCoreResultTest
                    E.&&. (tr E.?. BuildCoreResultSubmission E.==. E.just (E.val bsId)
                    E.||. E.isNothing (tr E.?. BuildCoreResultId)))
                E.where_ ( t E.^. ContestCoreTestContest E.==. E.val cId)
                E.orderBy [E.asc (t E.^. ContestCoreTestId)]
                return ( t, tr)
        performanceResults <- handlerToWidget $ runDB 
            -- [lsql|
            --     select ContestPerformanceTest.*, BuildPerformanceResult.* from ContestPerformanceTest
            --     left outer join BuildPerformanceResult on ContestPerformanceTest.id == BuildPerformanceResult.test
            --     where ContestPerformanceTest.contest == #{cId}
            --         and (BuildPerformanceResult.submission == #{Just bsId} or BuildPerformanceResult.id is null)
            --     order by ContestPerformanceTest.name asc
            -- |]
            $ E.select $ E.from $ \(t `E.LeftOuterJoin` tr) -> do
                E.on ( E.just (t E.^. ContestPerformanceTestId) E.==. tr E.?. BuildPerformanceResultTest
                    E.&&. ( tr E.?. BuildPerformanceResultSubmission E.==. E.just (E.val bsId)
                    E.||. E.isNothing (tr E.?. BuildPerformanceResultId)))
                E.where_ ( t E.^. ContestPerformanceTestContest E.==. E.val cId)
                E.orderBy [E.asc (t E.^. ContestPerformanceTestId)]
                return ( t, tr)
        optionalResults <- handlerToWidget $ runDB 
            -- [lsql|
            --     select ContestOptionalTest.*, BuildOptionalResult.* from ContestOptionalTest
            --     left outer join BuildOptionalResult on ContestOptionalTest.id == BuildOptionalResult.test
            --     where ContestOptionalTest.contest == #{cId}
            --         and (BuildOptionalResult.submission == #{Just bsId} or BuildOptionalResult.id is null)
            --     order by ContestOptionalTest.name asc
            -- |]
            $ E.select $ E.from $ \(t `E.LeftOuterJoin` tr) -> do
                E.on ( E.just (t E.^. ContestOptionalTestId) E.==. tr E.?. BuildOptionalResultTest
                    E.&&. ( tr E.?. BuildOptionalResultSubmission E.==. E.just (E.val bsId) 
                    E.||. E.isNothing (tr E.?. BuildOptionalResultId)))
                E.where_ ( t E.^. ContestOptionalTestContest E.==. E.val cId)
                E.orderBy [E.asc (t E.^. ContestOptionalTestId)]
                return ( t, tr)
        [whamlet|
            <h3>
                Test Results
        |]
        if ((length coreResults) + (length performanceResults) + (length optionalResults)) == 0 then
            [whamlet|
                <p>
                    No tests found.
            |]
        else
            let cores = mconcat $ map renderCores coreResults :: Widget in
            let perfs = mconcat $ map renderPerfs performanceResults :: Widget in
            let opts = mconcat $ map renderOpts optionalResults :: Widget in
            [whamlet|
                <table class="table">
                    <thead>
                        <tr>
                            <th>
                                Test name
                            <th>
                                Test type
                            <th>
                                Result
                            <th>
                                Performance
                    <tbody>
                        ^{cores}
                        ^{perfs}
                        ^{opts}
            |]
    else
        mempty

    where
        -- testMessage :: Maybe Text -> (Widget, Html)
        testMessage _ True _ = 
            (mempty, mempty)
        testMessage _ False Nothing = 
            (mempty, mempty)
        testMessage rowId False (Just msg) =
            let row = [whamlet'|
                <tr>
                    <td .message-column colspan="4">
                        <div id="#{rowId}" .collapse>
                            <pre>
                                #{msg}
              |]
            in
            let button = [shamlet|
                <a href="##{rowId}" data-toggle="collapse" aria-expanded="false" aria-controls="#{rowId}" class="details-link">
                    (Details)
              |]
            in
            ( row, button)
