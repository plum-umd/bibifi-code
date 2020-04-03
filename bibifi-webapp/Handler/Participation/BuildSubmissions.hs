-- Reference: http://stackoverflow.com/questions/17147821/how-to-make-a-whole-row-in-a-table-clickable-as-a-link

module Handler.Participation.BuildSubmissions where

import Import
import qualified Participation
import PostDependencyType
import Submissions
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
                displayBuildSubmissionsTable False submissions

getParticipationBuildSubmissionR :: TeamContestId -> BuildSubmissionId -> Handler Html
getParticipationBuildSubmissionR tcId bsId = runLHandler $ do
    bs <- checkBuildSubmissionTeam tcId bsId
    Participation.layout Participation.BuildSubmissions tcId $ \_ teamcontest contest _ -> do
        [whamlet|
            <a href="@{ParticipationBuildSubmissionsR tcId}" type="button" class="btn btn-primary">
                Back
            <h2>
                Submission
        |]
        Widgets.buildSubmission (Entity bsId bs) (teamContestContest teamcontest) False

        -- Check if we can rerun submission.
        rerunSubmission <- canRerunBuildSubmission bs contest
        when rerunSubmission $
            rerunWidget bs

    where
        rerunWidget bs = do
            ( widget, enctype) <- handlerToWidget $ generateFormPost rerunForm
            [whamlet|
                <h3>
                    Rerun Submission
                <form method=post action="@{ParticipationBuildSubmissionRerunR tcId bsId}" enctype=#{enctype}>
                    ^{widget}
                    <button type="submit" class="btn btn-warning">
                        Rerun
            |]

checkBuildSubmissionTeam tcId bsId = do
    bs <- runDB $ get404 bsId
    when (buildSubmissionTeam bs /= tcId)
        notFound

    return bs

-- Determines if we can rerun submission.
canRerunBuildSubmission bs contest = do
    (Entity _ user) <- handlerToWidget requireAuth
    if not (userAdmin user) then
        return False
    else
        let status = buildSubmissionStatus bs in
        if not (status == BuildTimeout || status == BuildBuildFail || status == BuildBuilt) then
            return False
        else if development then
            return True
        else do
            now <- getCurrentTime
            return $ now <= contestBuildEnd contest
        
data RerunFormData = RerunFormData ()

rerunForm = identifyForm "rerun-build-submission" $ renderBootstrap3 BootstrapBasicForm $ RerunFormData
    <$> pure ()

postParticipationBuildSubmissionRerunR :: TeamContestId -> BuildSubmissionId -> Handler Html
postParticipationBuildSubmissionRerunR tcId bsId = runLHandler $ do
    bs <- checkBuildSubmissionTeam tcId bsId
    Participation.layout Participation.BuildSubmissions tcId $ \_ teamcontest contest _ -> do
        ((res, widget), enctype) <- handlerToWidget $ runFormPost rerunForm
        case res of
            FormMissing ->
                errorHandler
            FormFailure _ ->
                errorHandler
            FormSuccess _ -> do
                -- Check if we can rerun this submission.
                canRerun <- canRerunBuildSubmission bs contest
                if not canRerun then
                    errorHandler
                else do
                    handlerToWidget $ runDB $ update bsId [BuildSubmissionStatus =. BuildPending, BuildSubmissionStdout =. Nothing, BuildSubmissionStderr =. Nothing]

                    setMessage [shamlet|
                        <div .container>
                            <div .alert .alert-success>
                                Rerunning submission.
                    |]
                    redirect $ ParticipationBuildSubmissionR tcId bsId

    where
        errorHandler = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not rerun submission.
            |]
            redirect $ ParticipationBuildSubmissionR tcId bsId
