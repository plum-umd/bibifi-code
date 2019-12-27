{-# LANGUAGE FlexibleContexts #-}

module Handler.Admin.Contest.BuildSubmissions where

import qualified Admin
import Import
import Submissions

getAdminContestBuildSubmissionsR :: Text -> Handler Html
getAdminContestBuildSubmissionsR url = runLHandler $ Admin.layoutContest url $ \(Entity contestId _) -> do
    Admin.setTitle "Build Submissions"

    filter <- getFilter contestId
    -- JP: Better to use a join? 
    submissions <- handlerToWidget $ runDB $ selectList filter []

    [whamlet|
        <a href="@{AdminContestR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Build Submissions
    |]

    if null submissions then
        [whamlet|
            <p>
                There are no build submissions.
        |]
    else
        displayBuildSubmissionsTable True submissions
        

    where
        getFilter contestId = do
            -- Get teams.
            teams <- fmap (fmap entityKey) $ handlerToWidget $ runDB $ selectList [TeamContestContest ==. contestId] []
            -- TODO: Add filters by status, team, etc. XXX

            let base = [BuildSubmissionTeam <-. teams]

            return base
