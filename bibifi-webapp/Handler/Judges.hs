module Handler.Judges where

import qualified Data.List as List
import qualified Data.Time.Clock as Clock
import qualified Config

import Import
import qualified Judges
import qualified Settings.Judges as Settings

-- Redirect to active contest.
getRedirectJudgesR :: Handler Html
getRedirectJudgesR = runLHandler $ do
    activeContestM <- Config.getConfig Config.DefaultContest
    case activeContestM of
        Just activeContest ->
            redirect $ JudgesR activeContest
        Nothing ->
            notFound

getJudgesR :: Text -> Handler Html
getJudgesR contestUrl = runLHandler $ Judges.layout contestUrl $ \uId (Entity cId c) _ -> do
    displayCompleted <- do
        disp <- lookupGetParam "completed"
        return ( disp == (Just "t"))
    -- TODO: Display instructions/link to rules.
    let completedButton = if displayCompleted then
            [whamlet'|
                <a href="@{JudgesR contestUrl}" type="button" class="btn btn-primary pull-right">
                    Hide completed
                <div class="clearfix">
            |]
          else
            [whamlet'|
                <a href="@{JudgesR contestUrl}?completed=t" type="button" class="btn btn-primary pull-right">
                    Show completed
                <div class="clearfix">
            |]  
    (buildJudgements, breakJudgements, fixJudgements) <- handlerToWidget $ runDB $ do
        judgeM <- getBy $ UniqueContestJudge uId cId
        case judgeM of
            Nothing ->
                notFound
            Just (Entity jId _) ->
                if displayCompleted then do
                    a <- selectList [BuildJudgementJudge ==. jId] []
                    b <- selectList [BreakJudgementJudge ==. jId] []
                    c <- selectList [FixJudgementJudge ==. jId] []
                    return ( a, b, c)
                else do
                    a <- selectList [BuildJudgementJudge ==. jId, BuildJudgementRuling ==. Nothing] []
                    b <- selectList [BreakJudgementJudge ==. jId, BreakJudgementRuling ==. Nothing] []
                    c <- selectList [FixJudgementJudge ==. jId, FixJudgementRuling ==. Nothing] []
                    return ( a, b, c)
    tableBody <- 
        -- Check if no judgements are in the queue.
        if (List.length buildJudgements) + (List.length breakJudgements) + (List.length fixJudgements) == 0 then
            return $ [whamlet'|
                <tr>
                    <td colspan="4">
                        No jobs found. 
            |]
        else do
            -- Set due dates 3 days after round ends.
            buildDueDate <- displayTime $ Clock.addUTCTime (2*24*60*60) (contestBuildEnd c)
            breakDueDate <- displayTime $ Clock.addUTCTime (2*24*60*60) (contestBreakEnd c)
            let buildRows = mconcat $ map (\(Entity jId j) -> 
                    [whamlet'|
                        <tr class="clickable" href="@{JudgesBuildItR contestUrl jId}">
                            <td>
                                build-#{keyToInt jId}
                            <td>
                                Build-it
                            <td>
                                #{buildDueDate}
                            <td>
                                #{prettyPrintStatus (buildJudgementRuling j)}
                    |]
                  ) buildJudgements
            let breakRows = mconcat $ map (\(Entity jId j) -> 
                    [whamlet'|
                        <tr class="clickable" href="@{JudgesBreakItR contestUrl jId}">
                            <td>
                                break-#{keyToInt jId}
                            <td>
                                Break-it
                            <td>
                                #{breakDueDate}
                            <td>
                                #{prettyPrintStatus (breakJudgementRuling j)}
                    |]
                  ) breakJudgements
            return $ [whamlet'|
                ^{buildRows}
                ^{breakRows}
            |]
    [whamlet|
        ^{completedButton}
        <table class="table table-hover">
            <thead>
                <tr>
                    <th>
                        Job id
                    <th>
                        Round
                    <th>
                        Due
                    <th>
                        Status
            <tbody>
                ^{tableBody}
        <h2>
            Repository Information
        <form class="form-horizontal">
            <div class="form-group">
                <label class="col-xs-3 control-label">
                    Bitbucket account
                <div class="col-xs-9">
                    <p class="form-control-static">
                        #{Settings.gitAccount}
            <div class="form-group">
                <label class="col-xs-3 control-label">
                    Bitbucket password
                <div class="col-xs-9">
                    <p class="form-control-static">
                        #{Settings.gitPassword}
            <div class="form-group">
                <label class="col-xs-3 control-label">
                    Repository URL
                <div class="col-xs-9">
                    <p class="form-control-static">
                        #{Settings.gitUrl}

    |]
    clickableDiv
        where
            prettyPrintStatus s = case s of
                Nothing ->
                    [shamlet|
                        <span class="text-danger">
                            Incomplete
                    |]
                Just True ->
                    [shamlet|
                        <span class="text-success">
                            Complete (Passed)
                    |]
                Just False ->
                    [shamlet|
                        <span class="text-warning">
                            Complete (Failed)
                    |]

