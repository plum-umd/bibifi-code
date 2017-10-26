module Handler.Admin.Contest.FixSubmissions where

import qualified Admin
import Import
import Submissions

getAdminContestFixSubmissionsR :: Text -> Handler Html
getAdminContestFixSubmissionsR url = runLHandler $ Admin.layoutContest url $ \(Entity contestId _) -> do
    Admin.setTitle "Build Submissions"

    filter <- getFilter contestId

    submissions <- handlerToWidget $ runDB $ selectList filter []

    [whamlet|
        <a href="@{AdminContestR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Break Submissions
    |]

    if null submissions then
        [whamlet|
            <p>
                There are no fix submissions.
        |]
    else
        displayFixSubmissionsTable True submissions

    where
        getFilter contestId = do
            -- Get teams.
            teams <- fmap (fmap entityKey) $ handlerToWidget $ runDB $ selectList [TeamContestContest ==. contestId] []
            -- TODO: Add filters by status, team, etc. XXX

            let base = [FixSubmissionTeam <-. teams]

            return base
