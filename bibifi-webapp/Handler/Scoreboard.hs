-- Icon: http://www.ajaxload.info/

module Handler.Scoreboard where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Import
import qualified Widgets

getScoreboardR :: Handler Html
getScoreboardR = runLHandler $
    getScoreboard Nothing

getSpecificScoreboardR :: Text -> Handler Html
getSpecificScoreboardR url = runLHandler $
    getScoreboard $ Just url

getScoreboard :: Maybe Text -> LHandler Html
getScoreboard url = do
    contest <- retrieveContest url
    let title = generatePageTitle contest "Scoreboard"
    customLayout contest $ do
        lLift $ setTitle $ toHtml title
        contestTemplate contest "Scoreboard" showScoreboard

showScoreboard :: Entity Contest -> LWidget
showScoreboard c =
    let Entity cId contest = c in
    let url = contestUrl contest in
    lLift $ do
    now <- getCurrentTime
    if now < (contestBuildStart contest) then
        [whamlet'|
            <div class="row">
                <div class="col-md-12">
                    The contest has not started yet.
        |]
    else
        do
        toWidget [lucius|
            img.loading {
                margin: 10px 0px;
            }
            
            .axis {
                fill: #999;
            }

            .axis path,
            .axis line {
                fill: none;
                stroke: #999;
                shape-rendering: crispEdges;
                z-index: 10;
            }
            
            .line {
                fill: none;
                stroke: #e6550d;
                stroke-width: 1.5px;
            }

        |]

        -- Hide fix-it if not fall 2014 contest.
        -- unless ( url == "fall2014") $ toWidget
        --     [lucius|
        --         .fixit {
        --             display:none
        --         }
        --     |]
            
        addScript $ StaticR js_d3_v3_min_js
        addScript $ StaticR js_bibifi_scoreboard_js
        [whamlet'|
            <div class="row">
                <div class="col-md-12">
                    <h2>
                        Builder Scores
                        <small>
                            <a href="@{PerformanceRankingsR url}">
                                Performance Rankings
                    <div id="builder-scores">
                        <div class="text-center">
                            <img class="loading" src="@{StaticR img_loading_gif}">
                            <p class="text-muted">
                                Loading...
                    <h2>
                        Breaker Scores
                    <div id="breaker-scores">
                        <div class="text-center">
                            <img class="loading" src="@{StaticR img_loading_gif}">
                            <p class="text-muted">
                                Loading...
                    <p class="text-muted">
                        Note: The scoreboard automatically refreshes every 30 seconds. Asterisks (*) indicate professional breaker teams. 
        |]
        clickableDiv
        toWidget [julius|
            $(document).ready( function() {
                bibifi.scoreboard.init( "@{ScoresR (contestUrl contest)}");
            });
        |]

data Meta = Meta { 
        start :: UTCTime,
        end :: UTCTime,
        teams :: [(TeamContestId, Text, Bool)]
    }
data Scores = Scores {
        meta :: Meta,
        builder :: [TeamBuildScore],
        breaker :: [TeamBreakScore]
    }

instance ToJSON Meta where
    toJSON (Meta start end' teams') = 
      let printTeam acc ( tcId, name, prof) = 
            let n = keyToInt tcId in
            let k = T.pack $ show n in
            let v = object ["name" .= (toJSON name), "professional" .= (toJSON prof)] in
            (k .= v):acc
      in
      -- let teams = object $ foldl printTeam [] ((Key (PersistInt64 132),T.pack "t' bad ^ & 1 es\"t"):teams') in
      let teams = object $ foldl printTeam [] teams' in
      object [
        "start" .= toJSON start,
        "end" .= toJSON end',
        "teams" .= teams
      ]

instance ToJSON Scores where
    toJSON (Scores meta' builder breaker) = object [
        "meta" .= toJSON meta',
        "builder" .= toJSON builder,
        "breaker" .= toJSON breaker
      ]

getScoresR :: Text -> Handler Aeson.Value
getScoresR cUrl = runLHandler $ do
    res <- runDB $ getBy $ UniqueContest cUrl
    case res of
        Nothing ->
            returnJson $ object []
        Just (Entity cId c) -> 
            let end' = addUTCTime (60*30) (contestFixEnd c) in
            do
            now <- getCurrentTime
            teams <- if now > (contestFixEnd c) then 
                    runDB [lsql| select TeamContest.id, Team.name, TeamContest.professional from Team inner join TeamContest on Team.id == TeamContest.team where TeamContest.contest == #{cId} and TeamContest.gitUrl != #{""}|]
                else
                    runDB [lsql| select TeamContest.id, Team.name, TeamContest.professional from Team inner join TeamContest on Team.id == TeamContest.team where TeamContest.contest == #{cId} |]

            let meta' = Meta (contestBuildStart c) end' teams
            
            -- builderScores' <- runDB [lsql| select TeamBuildScore.* from TeamBuildScore inner join TeamContest on TeamContest.id == TeamBuildScore.team where TeamContest.contest == #{cId} order by TeamBuildScore.timestamp asc|]
            -- let builderScores = fmap entityVal builderScores'
            builderScores <- fmap mconcat $ mapM (\(teamId, _, _) -> do
                    scores <- runDB [lsql| select TeamBuildScore.* from TeamBuildScore where TeamBuildScore.team == #{teamId} order by TeamBuildScore.id desc limit 1|]
                    return $ map entityVal scores
                ) teams
            
            -- breakerScores' <- runDB [lsql| select TeamBreakScore.* from TeamBreakScore inner join TeamContest on TeamContest.id == TeamBreakScore.team where TeamContest.contest == #{cId} order by TeamBreakScore.timestamp asc |]
            -- let breakerScores = fmap entityVal breakerScores'
            breakerScores <- fmap mconcat $ mapM (\(teamId, _, _) -> do
                    scores <- runDB [lsql|select TeamBreakScore.* from TeamBreakScore where TeamBreakScore.team == #{teamId} order by TeamBreakScore.id desc limit 1|]
                    return $ map entityVal scores
                ) teams

            returnJson $ Scores meta' builderScores breakerScores



getScoreBreakdownR :: TeamContestId -> Handler Html
getScoreBreakdownR tcId = runLHandler $ do
    tcM <- runDB $ pGet tcId
    case tcM of 
        Nothing ->
            lLift $ notFound
        Just tc -> do
            let cId' = pTeamContestContest tc
            contest' <- runDB $ get cId'
            let contestM = maybe Nothing (Just . (Entity cId')) contest'
            let title = generatePageTitle contestM "Score Breakdown"
            customLayout contestM $ do
                lLift $ setTitle $ toHtml title
                contestTemplate contestM "Score Breakdown" $ \(Entity cId contest) -> do
                    now <- getCurrentTime
                    if now < (contestBuildStart contest) then
                        [whamlet|
                            <div class="row">
                                <div class="col-md-12">
                                    The contest has not started yet.
                        |]
                    else do
                        [whamlet|
                            <a href="@{SpecificScoreboardR (contestUrl contest)}" type="button" class="btn btn-primary">
                                Back
                            <h2>
                                Latest Build-it Submission
                        |]
                    -- Get team's latest submission. 
                    submissionM <- handlerToWidget $ runDB $ selectFirst [BuildSubmissionTeam ==. tcId] [Desc BuildSubmissionId] -- Desc BuildSubmissionTimestamp
                    case submissionM of
                        Nothing ->
                            [whamlet|
                                <div class="row">
                                    <div class="col-md-12">
                                        This team has not made any build-it submissions. 
                            |]
                        Just submission ->
                            Widgets.buildSubmission submission cId True
