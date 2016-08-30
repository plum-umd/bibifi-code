-- Reference: http://stackoverflow.com/questions/17147821/how-to-make-a-whole-row-in-a-table-clickable-as-a-link

module Handler.Participation.BuildSubmissions where

import Import
import qualified Participation
import PostDependencyType
import qualified Widgets

getParticipationBuildSubmissionsR :: TeamContestId -> Handler Html
getParticipationBuildSubmissionsR tcId = runLHandler $ 
    Participation.layout Participation.BuildSubmissions tcId $ \_ _ _ _ -> do
        submissions <- handlerToWidget $ runDB $ selectList [BuildSubmissionTeam ==. tcId] [Desc BuildSubmissionId]
        case submissions of
            [] ->
                [whamlet|
                    <p>
                        No submissions were found. If you have made submissions, please ensure your git url is correct on the information page.
                |]
            _ ->
                let row (Entity sId s) = do
                    let status = prettyBuildStatus $ buildSubmissionStatus s
                    time <- lift $ displayTime $ buildSubmissionTimestamp s
                    [whamlet'|
                        <tr class="clickable" href="@{ParticipationBuildSubmissionR tcId sId}">
                            <td>
                                #{buildSubmissionCommitHash s}
                            <td>
                                #{time}
                            <td>
                                #{status}
                    |]
                in
                do
                let rows = mconcat $ map row submissions
                [whamlet|
                    <table class="table table-hover">
                        <thead>
                            <tr>
                                <th>
                                    Submission hash
                                <th>
                                    Timestamp
                                <th>
                                    Status
                        <tbody>
                            ^{rows}
                |]
                clickableDiv

getParticipationBuildSubmissionR :: TeamContestId -> BuildSubmissionId -> Handler Html
getParticipationBuildSubmissionR tcId bsId = runLHandler $ do
    res <- runDB $ get bsId
    case res of 
        Nothing ->
            notFound
        Just bs ->
            if (buildSubmissionTeam bs) /= tcId then
                notFound
            else
                Participation.layout Participation.BuildSubmissions tcId $ \_ teamcontest _ _ -> do
                    [whamlet|
                        <a href="@{ParticipationBuildSubmissionsR tcId}" type="button" class="btn btn-primary">
                            Back
                        <h2>
                            Submission
                    |]
                    Widgets.buildSubmission (Entity bsId bs) (teamContestContest teamcontest) False
                    -- let status = prettyBuildStatus $ buildSubmissionStatus bs
                    -- time <- lift $ displayTime $ buildSubmissionTimestamp bs
                    -- [whamlet|
                    --     <a href="@{ParticipationBuildSubmissionsR tcId}" type="button" class="btn btn-primary">
                    --         Back
                    --     <h2>
                    --         Submission
                    --     <form class="form-horizontal">
                    --         <div class="form-group">
                    --             <label class="col-sm-2 control-label">
                    --                 Submission hash
                    --             <div class="col-sm-10">
                    --                 <p class="form-control-static">
                    --                     #{buildSubmissionCommitHash bs}
                    --         <div class="form-group">
                    --             <label class="col-sm-2 control-label">
                    --                 Timestamp
                    --             <div class="col-sm-10">
                    --                 <p class="form-control-static">
                    --                     #{time}
                    --         <div class="form-group">
                    --             <label class="col-sm-2 control-label">
                    --                 Status
                    --             <div class="col-sm-10">
                    --                 <p class="form-control-static">
                    --                     #{status}
                    -- |]
                    -- if (buildSubmissionStatus bs) == BuildBuilt then
                    --     let renderCores ((Entity _ test), mbr') = 
                    --           let result = case mbr' of 
                    --                 Nothing ->
                    --                     prettyPassResult False
                    --                 Just (Entity _ mbr) ->
                    --                     prettyPassResult $ buildCoreResultPass mbr
                    --           in
                    --           [whamlet|
                    --               <tr>
                    --                   <td>
                    --                       #{contestCoreTestName test}
                    --                   <td>
                    --                       Correctness
                    --                   <td>
                    --                       #{result}
                    --                   <td>
                    --                       #{dash}
                    --           |]
                    --     in
                    --     let renderOpts ((Entity _ test), mbr') =
                    --           let result = case mbr' of 
                    --                 Nothing ->
                    --                     prettyPassResult False
                    --                 Just (Entity _ mbr) ->
                    --                     prettyPassResult $ buildOptionalResultPass mbr
                    --           in
                    --           [whamlet|
                    --               <tr>
                    --                   <td>
                    --                       #{contestOptionalTestName test}
                    --                   <td>
                    --                       Optional
                    --                   <td>
                    --                       #{result}
                    --                   <td>
                    --                       #{dash}
                    --           |]
                    --     in
                    --     let renderPerfs ((Entity _ test), mbr') =
                    --           let (result, period) = case mbr' of
                    --                 Nothing ->
                    --                     (prettyPassResult False, dash)
                    --                 Just (Entity _ mbr) ->
                    --                     case buildPerformanceResultTime mbr of
                    --                         Nothing ->
                    --                             ( prettyPassResult False, dash)
                    --                         Just t ->
                    --                             let period' = [shamlet|#{t}|] in
                    --                             ( prettyPassResult True, period')
                    --           in
                    --           [whamlet|
                    --               <tr>
                    --                   <td>
                    --                       #{contestPerformanceTestName test}
                    --                   <td>
                    --                       Performance
                    --                   <td>
                    --                       #{result}
                    --                   <td>
                    --                       #{period}
                    --           |]
                    --     in
                    --     do
                    --     let cId = teamContestContest teamcontest
                    --     coreResults <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.LeftOuterJoin` tr) -> do
                    --         E.on ( E.just (t E.^. ContestCoreTestId) E.==. tr E.?. BuildCoreResultTest)
                    --         E.where_ ( t E.^. ContestCoreTestContest E.==. E.val cId 
                    --             E.&&. (tr E.?. BuildCoreResultSubmission E.==. E.just (E.val bsId)
                    --             E.||. E.isNothing (tr E.?. BuildCoreResultId)))
                    --         E.orderBy [E.asc (t E.^. ContestCoreTestName)]
                    --         return ( t, tr)
                    --     performanceResults <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.LeftOuterJoin` tr) -> do
                    --         E.on ( E.just (t E.^. ContestPerformanceTestId) E.==. tr E.?. BuildPerformanceResultTest)
                    --         E.where_ ( t E.^. ContestPerformanceTestContest E.==. E.val cId
                    --             E.&&. ( tr E.?. BuildPerformanceResultSubmission E.==. E.just (E.val bsId)
                    --             E.||. E.isNothing (tr E.?. BuildPerformanceResultId)))
                    --         E.orderBy [E.asc (t E.^. ContestPerformanceTestName)]
                    --         return ( t, tr)
                    --     optionalResults <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.LeftOuterJoin` tr) -> do
                    --         E.on ( E.just (t E.^. ContestOptionalTestId) E.==. tr E.?. BuildOptionalResultTest)
                    --         E.where_ ( t E.^. ContestOptionalTestContest E.==. E.val cId
                    --             E.&&. ( tr E.?. BuildOptionalResultSubmission E.==. E.just (E.val bsId) 
                    --             E.||. E.isNothing (tr E.?. BuildOptionalResultId)))
                    --         E.orderBy [E.asc (t E.^. ContestOptionalTestName)]
                    --         return ( t, tr)
                    --     [whamlet|
                    --         <h2>
                    --             Test Results
                    --     |]
                    --     if ((length coreResults) + (length performanceResults) + (length optionalResults)) == 0 then
                    --         [whamlet|
                    --             <p>
                    --                 No tests found.
                    --         |]
                    --     else
                    --         let cores = mconcat $ map renderCores coreResults in
                    --         let perfs = mconcat $ map renderPerfs performanceResults in
                    --         let opts = mconcat $ map renderOpts optionalResults in
                    --         [whamlet|
                    --             <table class="table table-hover">
                    --                 <thead>
                    --                     <tr>
                    --                         <th>
                    --                             Test name
                    --                         <th>
                    --                             Test type
                    --                         <th>
                    --                             Result
                    --                         <th>
                    --                             Performance
                    --                 <tbody>
                    --                     ^{cores}
                    --                     ^{perfs}
                    --                     ^{opts}
                    --         |]
                    -- else
                    --     mempty
