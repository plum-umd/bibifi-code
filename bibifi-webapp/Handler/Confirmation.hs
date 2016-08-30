module Handler.Confirmation where

import Import

getConfirmationR :: Text -> Handler Html
getConfirmationR code = runLHandler $ do
    res <- runDB $ getBy $ UniqueConfirmation code
    case res of
        Nothing -> defaultLayout [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="text-danger">
                        Sorry, this confirmation code is invalid.
        |]
        Just (Entity confId _) -> do
            runDB $ delete confId
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Successfully confirmed your account!
            |]
            redirectUltDest AnnouncementsR -- TODO: change?
