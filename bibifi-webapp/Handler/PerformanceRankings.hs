{-# LANGUAGE ScopedTypeVariables #-}
module Handler.PerformanceRankings where

import Control.Monad
import Data.List (sortBy)
import qualified Database.Esqueleto as E

import BuildSubmissions
import Import

getPerformanceRankingsR :: Text -> Handler Html
getPerformanceRankingsR url = runLHandler $ do
    contest' <- retrieveContest $ Just url
    let title = generatePageTitle contest' "Performance Rankings"
    customLayout contest' $ do
        setTitle $ toHtml title
        contestTemplate contest' "Performance Rankings" $ \(Entity cId contest) ->
            do
            now <- getCurrentTime
            if now < (contestBuildStart contest) then
                [whamlet|
                    <div class="row">
                        <div class="col-md-12">
                            The contest has not started yet.
                |]
            else do
                -- Get list of performance tests for this contest.
                tests <- handlerToWidget $ runDB $ selectList [ContestPerformanceTestContest ==. cId] [Asc ContestPerformanceTestId]
                let rows = 
                      let row (Entity tId t) = 
                            [whamlet'|
                                <tr href="@{SpecificPerformanceRankingsR url tId}" .clickable>
                                    <td>
                                        #{contestPerformanceTestName t}
                            |]
                      in
                      mconcat $ map row tests
                [whamlet|
                    <a href="@{SpecificScoreboardR url}" type="button" class="btn btn-primary">
                        Back
                    <h2>
                        Performance tests
                    <table class="table table-hover">
                        <thead>
                            <tr>
                                <th>
                                    Test name
                        <tbody>
                            ^{rows}
                |]
                clickableDiv

getSpecificPerformanceRankingsR :: Text -> ContestPerformanceTestId -> Handler Html
getSpecificPerformanceRankingsR url ptId = runLHandler $ do
    contestM <- retrieveContest $ Just url
    let title = generatePageTitle contestM "Performance Rankings"
    customLayout contestM $ do
        setTitle $ toHtml title
        contestTemplate contestM "Performance Rankings" $ \(Entity cId contest) -> do
            ptM <- handlerToWidget $ runDB $ get ptId
            case ptM of
                Nothing ->
                    notFound
                Just pt | cId /= (contestPerformanceTestContest pt) ->
                    notFound
                Just pt -> do
                    now <- getCurrentTime
                    if now < (contestBuildStart contest) then
                        [whamlet|
                            <div class="row">
                                <div class="col-md-12">
                                    The contest has not started yet.
                        |]
                    else do
                        -- TODO: make this faster...
                        rows <- handlerToWidget $ do
                            res <- runDB $ getLatestBuildSubmissions cId $ \tc bs ->
                                return (tc E.^. TeamContestTeam, bs E.^. BuildSubmissionId)
                            -- $ E.select $ E.from $ \(E.InnerJoin tc (E.LeftOuterJoin tbs tbs')) -> do
                            --     E.on (E.just (tbs E.^. BuildSubmissionTeam) E.==. tbs' E.?. BuildSubmissionTeam E.&&. E.just (tbs E.^. BuildSubmissionTimestamp) E.<. tbs' E.?. BuildSubmissionTimestamp)
                            --     E.on (tc E.^. TeamContestId E.==. tbs E.^. BuildSubmissionTeam)
                            --     E.where_ ( tc E.^. TeamContestContest E.==. E.val cId E.&&. E.isNothing (tbs' E.?. BuildSubmissionTeam) E.&&. tc E.^. TeamContestProfessional E.==. E.val False)
                            --     return (tc E.^. TeamContestTeam, tbs E.^. BuildSubmissionId)
                            res' <- mapM (\(E.Value tId, E.Value sId) -> do
                                    tM <- runDB $ get tId
                                    sM <- runDB $ getBy $ UniqueBuildPerformanceSubmission sId ptId
                                    let team = case tM of
                                          Nothing -> "error"
                                          Just t -> teamName t
                                    let p = case sM of
                                          Nothing -> Nothing
                                          Just (Entity _ s) -> buildPerformanceResultTime s
                                    return ( team, p)
                                ) res
                            return $ mconcat $ fmap (\( team, p') ->
                                let p = case p' of
                                      Nothing -> dash
                                      Just t -> [shamlet|#{t}|]
                                in
                                [whamlet'|
                                    <tr>
                                        <td>
                                            #{team}
                                        <td>
                                            #{p}
                                            
                                |]
                              ) $ sortBy (\p1 p2 -> case ( snd p1, snd p2) of
                                    (Just t1, Just t2) -> compare t1 t2
                                    (Just _, Nothing) -> LT
                                    (Nothing, Just _) -> GT
                                    (Nothing, Nothing) -> EQ
                                ) res'
                        let header = 
                                if contestPerformanceTestOptional pt then
                                    "Performance test" :: String
                                else
                                    "Performance test*"
                        [whamlet|
                            <a href="@{PerformanceRankingsR url}" type="button" class="btn btn-primary">
                                Back
                            <h2>
                                #{header}
                            <form class="form-horizontal">
                                <div class="form-group">
                                    <label class="col-sm-2 control-label">
                                        Test name
                                    <div class="col-sm-10">
                                        <p class="form-control-static">
                                            #{contestPerformanceTestName pt}
                            <table class="table table-hover">
                                <thead>
                                    <tr>
                                        <th>
                                            Team
                                        <th>
                                            Performance
                                <tbody>
                                    ^{rows}
                        |]
