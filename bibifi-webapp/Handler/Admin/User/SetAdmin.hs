module Handler.Admin.User.SetAdmin where

import qualified Admin
import Import

data FormData = FormData ()

form = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> pure ()

generateWidget :: Widget -> Enctype -> UserId -> LWidget
generateWidget widget enctype uId = do
    Admin.setTitle "Set Admin"
    userId <- handlerToWidget requireAuthId
    user <- handlerToWidget $ runDB $ get404 uId
    [whamlet|
        <a href="@{AdminUserR uId}" type="button" class="btn btn-primary">
            Back
        <h2>
            Set Admin Privileges
    |]
    if userId == uId then
        [whamlet|
            <p>
                You cannot revoke your own admin privileges.
        |]
    else if userAdmin user then
        formWidget "This user is currently an admin." "Remove admin privileges."
    else
        formWidget "This user is not an admin." "Grant admin privileges."

    where
        formWidget :: Text -> Text -> LWidget
        formWidget txt btn = 
            [whamlet|
                <p>
                    #{txt}
                <form method=post action=@{AdminUserSetAdminR uId} enctype=#{enctype} class="form-basic" role="form">
                    ^{widget}
                    <div .form-group .optional>
                        <button .btn .btn-primary type=submit>
                            #{btn}
            |]

getAdminUserSetAdminR :: UserId -> Handler Html
getAdminUserSetAdminR uId = runLHandler $ Admin.layout Admin.Users $ do
    ( widget, enctype) <- handlerToWidget $ generateFormPost form
    generateWidget widget enctype uId

postAdminUserSetAdminR :: UserId -> Handler Html
postAdminUserSetAdminR uId = runLHandler $ Admin.layout Admin.Users $ do
    ((res, widget), enctype) <- handlerToWidget $ runFormPost form
    case res of
        FormMissing ->
            errorHandler widget enctype
        FormFailure _ ->
            errorHandler widget enctype
        FormSuccess _ -> do
            currentUserId  <- handlerToWidget requireAuthId
            -- Check if self.
            if currentUserId == uId then do
                setMessage [shamlet|
                    <div class="container">
                        <div class="alert alert-danger">
                            You cannot revoke your own admin privileges.
                |]
                errorHandler widget enctype
            else do
                -- Update database.
                handlerToWidget $ runDB $ do
                    user <- get404 uId
                    update uId [UserAdmin =. not (userAdmin user)]

                -- Set message.
                setMessage [shamlet|
                    <div class="container">
                        <div class="alert alert-success">
                            Updated admin privileges.
                |]

                -- Redirect.
                redirect $ AdminUserSetAdminR uId

    where
        errorHandler widget enctype = generateWidget widget enctype uId
