module Handler.Participation.OracleSubmissions where

import Control.Monad
import Import
import PostDependencyType
import qualified Participation

getParticipationOracleSubmissionsR :: TeamContestId -> Handler Html
getParticipationOracleSubmissionsR tcId = runLHandler $ 
    Participation.layout Participation.Oracle tcId $ \_ _ contest _ -> do
        now <- getCurrentTime
        if not development && now < contestBuildStart contest then
            [whamlet|The contest has not started yet.|]
        else do
            -- Display link to submissions. 
            [whamlet|
                <p .lead>
                    You can make submissions to the oracle implementation, which will provide the expected output for the given input. Make a submission <a href="@{ParticipationOracleSubmissionCreateR tcId}">here</a>.
            |]
            -- Get submissions. 
            submissions <- handlerToWidget $ runDB $ selectList [OracleSubmissionTeam ==. tcId] [Desc OracleSubmissionTimestamp]
            when ( submissions /= []) $ do
                let row (Entity sId s) = do
                        let status = prettyOracleStatus $ oracleSubmissionStatus s
                        time <- displayTime $ oracleSubmissionTimestamp s
                        return [hamlet|
                            <tr .clickable href="@{ParticipationOracleSubmissionR tcId sId}">
                                <td>
                                    #{oracleSubmissionName s}
                                <td>
                                    #{time}
                                <td>
                                    #{status}
                        |]
                        
                rows <- mconcat <$> mapM row submissions
                [whamlet|
                    <table class="table table-hover">
                        <thead>
                            <tr>
                                <th>
                                    Submission
                                <th>
                                    Timestamp
                                <th>
                                    Status
                        <tbody>
                            ^{rows}
                |]
                clickableDiv
        
getParticipationOracleSubmissionR :: TeamContestId -> OracleSubmissionId -> Handler Html
getParticipationOracleSubmissionR tcId osId = runLHandler $ do
    res <- runDB $ get osId
    case res of
        Nothing -> 
            notFound
        Just os ->
            if (oracleSubmissionTeam os) /= tcId then
                notFound
            else
                Participation.layout Participation.Oracle tcId $ \_ _ _ _ ->
                    let output = case oracleSubmissionOutput os of
                            Nothing -> 
                                mempty
                            Just output -> [whamlet'|
                                <div class="form-group">
                                    <label class="col-xs-3 control-label">
                                        Output
                                    <div class="col-xs-9">
                                        <pre class="form-control-static">
                                            #{output}
                            |]
                    in
                    let refresh = case oracleSubmissionOutput os of
                            Nothing -> do
                                [whamlet'|
                                    <p class="text-muted">
                                        Note: This page automatically refreshes every 30 seconds.
                                |]
                                toWidget [julius|
                                    setTimeout(function(){
                                       window.location.reload(1);
                                    }, 30000);
                                |]
                            Just _ ->
                                return ()
                    in
                    [whamlet|
                        <a href="@{ParticipationOracleSubmissionsR tcId}" type="button" class="btn btn-primary">
                            Back
                        <h2>
                            Oracle submission
                        <form class="form-horizontal">
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Submission
                                <div class="col-xs-9">
                                    <p class="form-control-static">
                                        #{oracleSubmissionName os}
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Status
                                <div class="col-xs-9">
                                    <p class="form-control-static">
                                        #{prettyOracleStatus (oracleSubmissionStatus os)}
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Input
                                <div class="col-xs-9">
                                    <samp class="form-control-static">
                                        #{oracleSubmissionInput os}
                            ^{output}
                        ^{refresh}
                    |]
