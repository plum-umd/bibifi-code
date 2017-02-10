module Handler.Admin.Contest.BuildSubmissions where

getAdminContestBuildSubmissionsR :: Text -> Handler Html
getAdminContestBuildSubmissionsR url = runLHandler $ Admin.layoutContest url $ \(Entity _ _) -> do
    Admin.setTitle "Build Submissions"

    filter <- getFilter
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
                TODO
        |]
    else
        error "TODO"
        

    where
        getFilter = do
            -- Get teams.
            teams <- fmap (entityKey) $ handlerToWidget $ runDB $ selectList [TeamContestContest ==. ] []
            error "TODO"

            let base = [BuildSubmissionTeam <-. teams]

            return base
