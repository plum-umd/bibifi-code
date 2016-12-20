module Handler.Participation.FixSubmissions where

import Score

import Import
import qualified Participation
import PostDependencyType

getParticipationFixSubmissionsR :: TeamContestId -> Handler Html
getParticipationFixSubmissionsR tcId = runLHandler $ 
    Participation.layout Participation.FixSubmissions tcId $ \_ _ contest _ -> do
        now <- getCurrentTime
        if not development && now < (contestFixStart contest) then
            [whamlet|
                <p>
                    The fix-it round has not started yet.
            |]
        else do
            submissions <- handlerToWidget $ runDB $ selectList [FixSubmissionTeam ==. tcId] [Desc FixSubmissionId]
            case submissions of
                [] ->
                    [whamlet|
                        <p>
                            No submissions were found. If you have made submissions, please ensure your git url is correct on the information page.
                    |]
                _ ->
                    let row (Entity sId s) = do
                          let status = prettyFixStatus $ fixSubmissionStatus s
                          let result = prettyFixResult $ fixSubmissionResult s
                          time <- lLift $ lift $ displayTime $ fixSubmissionTimestamp s
                          return $ [whamlet'|
                            <tr .clickable href="@{ParticipationFixSubmissionR tcId sId}">
                                <td>
                                    #{fixSubmissionName s}
                                <td>
                                    #{time}
                                <td>
                                    #{status}
                                <td>
                                    #{result}
                          |]
                    in
                    do
                    rows <- mapM row submissions
                    [whamlet|
                        <table class="table table-hover">
                            <thead>
                                <tr>
                                    <th>
                                        Fix name
                                    <th>
                                        Timestamp
                                    <th>
                                        Status
                                    <th>
                                        Result
                            <tbody>
                                ^{mconcat rows}
                    |]
            [whamlet|
                <h3>
                    Break submissions
                <p>
                    You can find break submissions in <a href="/static/doc/#{contestUrl contest}/fix.zip">this file</a>. There is folder for each team, which contains the break tests submitted by that team. 
            |]
            clickableDiv

getParticipationFixSubmissionR :: TeamContestId -> FixSubmissionId -> Handler Html
getParticipationFixSubmissionR tcId fsId = runLHandler $ do
    res <- runDB $ get fsId
    case res of 
        Nothing ->
            notFound
        Just fs ->
            if ( fixSubmissionTeam fs) /= tcId then
                notFound
            else
                Participation.layout Participation.FixSubmissions tcId $ \userId _ contest team -> do
                    time <- lLift $ lift $ displayTime $ fixSubmissionTimestamp fs
                    judgementW <- do
                        judgementM <- handlerToWidget $ runDB $ getBy $ UniqueFixJudgement fsId
                        return $ case judgementM of 
                            Nothing ->
                                mempty
                            Just (Entity jId j) ->
                                let ruling = case fixJudgementRuling j of
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
                                let comments = case fixJudgementComments j of
                                      Nothing ->
                                        dash
                                      Just c ->
                                        toHtml c
                                in
                                [whamlet'|
                                    <div class="form-group">
                                        <label class="col-xs-3 control-label">
                                            Judgment
                                        <div class="col-xs-9">
                                            <p class="form-control-static">
                                                #{ruling}
                                    <div class="form-group">
                                        <label class="col-xs-3 control-label">
                                            Judge comments
                                        <div class="col-xs-9">
                                            <p class="form-control-static">
                                                #{comments}
                                |]
                    let message = case fixSubmissionMessage fs of
                          Nothing -> dash
                          Just msg -> [shamlet|
                            <span>
                                #{msg}
                            |]
                    deleteW <- do
                        now <- getCurrentTime
                        -- Check that it's during the fix it round.
                        if not development && (now < contestFixStart contest || now > contestFixEnd contest) then
                            return mempty
                        else
                            -- Check if team leader.
                            if userId /= teamLeader team then
                                return [whamlet'|
                                    <h3>
                                        Delete
                                    <p>
                                        Only the team leader may delete a break submission. 
                                |]
                            else
                                -- Check that the fix is done testing.
                                let st = fixSubmissionStatus fs in
                                if st == FixPending || st == FixBuilding then
                                    return [whamlet'|
                                        <h3>
                                            Delete
                                        <p>
                                            You cannot delete a fix submission until it is finished testing. 
                                    |]
                                else do
                                    (deleteW, deleteE) <- handlerToWidget $ generateFormPost deleteFixSubmissionForm
                                    return [whamlet'|
                                        <h3>
                                            Delete
                                        <p>
                                            Delete this fix submission. 
                                        <p .text-danger>
                                            Warning: This cannot be undone!
                                        <form .form-inline role=form method=post action="@{ParticipationFixSubmissionDeleteR tcId fsId}" enctype=#{deleteE}>
                                            ^{deleteW}
                                    |]
                    -- Build output widget.
                    let buildOutputW = case (fixSubmissionStatus fs, fixSubmissionStdout fs, fixSubmissionStderr fs) of
                            (FixRejected, Just stdout, Just stderr) ->
                                [whamlet'|
                                    <h4>
                                        Standard Output
                                    <samp>
                                        #{stdout}
                                    <h4>
                                        Standard Error
                                    <samp>
                                        #{stderr}
                                |]
                            _ ->
                                mempty

                    [whamlet|
                        <a href="@{ParticipationFixSubmissionsR tcId}" type="button" class="btn btn-primary">
                            Back
                        <h2>
                            Fix Submission
                        <form class="form-horizontal">
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Fix name
                                <div class="col-xs-9">
                                    <p class="form-control-static">
                                        #{fixSubmissionName fs}
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Submission hash
                                <div class="col-xs-9">
                                    <p class="form-control-static">
                                        #{fixSubmissionCommitHash fs}
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
                                        #{prettyFixStatus (fixSubmissionStatus fs)}
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Result
                                <div class="col-xs-9">
                                    <p class="form-control-static">
                                        #{prettyFixResult (fixSubmissionResult fs)}
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Message
                                <div class="col-xs-9">
                                    <p class="form-control-static">
                                        #{message}
                            ^{judgementW}
                        ^{buildOutputW}
                        ^{deleteW}
                    |]

data DeleteForm = DeleteForm ()

deleteFixSubmissionForm :: Form DeleteForm
deleteFixSubmissionForm = renderBootstrap3 (BootstrapInlineForm) $ DeleteForm
    <$> pure ()
    <* bootstrapSubmit (BootstrapSubmit ("Delete"::Text) "btn btn-danger" [])

postParticipationFixSubmissionDeleteR :: TeamContestId -> FixSubmissionId -> Handler Html
postParticipationFixSubmissionDeleteR tcId fsId = runLHandler $ Participation.layout Participation.FixSubmissions tcId $ \userId teamContest contest team -> do
    ((res, widget), enctype) <- handlerToWidget $ runFormPost deleteFixSubmissionForm
    case res of
        FormFailure _msg -> 
            failureHandler
        FormMissing -> 
            failureHandler
        FormSuccess (DeleteForm ()) -> do
            res <- handlerToWidget $ runDB $ get fsId
            case res of 
                Nothing ->
                    failureHandler
                Just fs ->
                    if ( fixSubmissionTeam fs) /= tcId then
                        failureHandler
                    else do
                        -- Check that it's during the fix it round.
                        now <- getCurrentTime
                        if not development && (now < contestFixStart contest || now > contestFixEnd contest) then
                            failureHandler
                        else
                            -- Check if team leader.
                            if userId /= teamLeader team then
                                failureHandler
                            else
                                -- Check that the fix is done testing.
                                let st = fixSubmissionStatus fs in
                                if st == FixPending || st == FixBuilding then
                                    failureHandler
                                else do
                                    handlerToWidget $ runDB $ do
                                        deleteWhere [FixSubmissionBugsFix ==. fsId]
                                        delete fsId
                                    handlerToWidget $ rescoreFixRound $ teamContestContest teamContest
                                    setMessage [shamlet|
                                        <div class="container">
                                            <div class="alert alert-success">
                                                Successfully deleted fix submission.
                                    |]
                                    redirect $ ParticipationFixSubmissionsR tcId
                
    where
        failureHandler = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not delete fix submission.
            |]
            redirect $ ParticipationFixSubmissionR tcId fsId
