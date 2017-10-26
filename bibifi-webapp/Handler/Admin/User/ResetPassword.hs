module Handler.Admin.User.ResetPassword where

import Control.Applicative
import Yesod.Form.Bootstrap3
import Import
import qualified Admin
import Forms
import User

data ResetPost = ResetPost Text

resetForm :: Form ResetPost
resetForm = renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 2) (ColMd 0) (ColMd 4)) $ ResetPost
    <$> areq emailField (bfs' "Email") Nothing
    <*  bootstrapSubmit (BootstrapSubmit ("Submit"::Text) "btn-primary" [])


generateWidget :: Widget -> Enctype -> [Text] -> UserId -> LWidget
generateWidget widget enctype msg uId = do
    Admin.setTitle "Password Reset"
    let msgH = mconcat $ map displayError msg
    [whamlet|
        <a href="@{AdminUserR uId}" type="button" class="btn btn-primary">
            Back
        <h2>
            Reset Password
        ^{msgH}
        <form method=post action=@{AdminUserResetPasswordR uId} enctype=#{enctype} class="form-horizontal" role="form">
            ^{widget}
    |]

getAdminUserResetPasswordR :: UserId -> Handler Html
getAdminUserResetPasswordR uId = runLHandler $ Admin.layout Admin.Users $ do
    ( widget, enctype) <- handlerToWidget $ generateFormPost resetForm
    generateWidget widget enctype [] uId
    
postAdminUserResetPasswordR :: UserId -> Handler Html
postAdminUserResetPasswordR uId = runLHandler $ Admin.layout Admin.Users $ do
    ((res, widget), enctype) <- handlerToWidget $ runFormPost resetForm
    case res of 
        FormFailure _msg ->
            generateWidget widget enctype [] uId -- msg
        FormMissing ->
            generateWidget widget enctype [] uId
        FormSuccess (ResetPost email) -> do
            mUser <- handlerToWidget $ runDB $ get uId
            case mUser of
                Nothing ->
                    generateWidget widget enctype ["Sorry, that user does not exist."] uId
                Just user ->
                    -- Check that emails match.
                    if ( userEmail user /= email) then
                        generateWidget widget enctype ["The given email address does not match the one associated with this user's account. Please verify that this user actually requested a password reset."] uId
                    else do
                        handlerToWidget $ invitePasswordReset $ Entity uId user
                        setMessage [shamlet|
                            <div class="container">
                                <div class="alert alert-success">
                                    Successfully reset the user's password. A reset code has been emailed to the user.
                        |]
                        redirect (AdminUserR uId)
                        
        
