module Handler.Admin.Contest.BreakSubmissions where

import qualified Admin
import Import
import Submissions

getAdminContestBreakSubmissionsR :: Text -> Handler Html
getAdminContestBreakSubmissionsR url = runLHandler $ Admin.layoutContest url $ \(Entity contestId contest) -> do
    Admin.setTitle "Break Submissions"

    submissions <- getSubmissions contestId

    [whamlet|
        <a href="@{AdminContestR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Break Submissions
    |]

    if null submissions then
        [whamlet|
            <p>
                There are no break submissions.
        |]
    else
        displayBreakSubmissionsTable contest BreakSubmissionAdmin $ fmap (fmap Just) submissions


    where
        getSubmissions contestId = do
            -- Get teams.
            -- teams <- fmap (fmap entityKey) $ handlerToWidget $ runDB $ selectList [TeamContestContest ==. contestId] []
            -- TODO: Add filters by status, team, etc. XXX

            handlerToWidget $ runDB $ [lsql| select BreakSubmission.*, Team.name from BreakSubmission inner join TeamContest on BreakSubmission.team == TeamContest.id inner join Team on TeamContest.team == Team.id where TeamContest.contest == #{contestId} order by BreakSubmission.id desc |]

