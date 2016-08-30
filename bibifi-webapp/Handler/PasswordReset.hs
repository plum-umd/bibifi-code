module Handler.PasswordReset where

import Control.Applicative
import Yesod.Auth
import Yesod.Form.Bootstrap3
import Import hiding (renderBootstrap3)
import Forms
import User

data FormData = FormData Text Text Text

resetForm :: Text -> Form FormData 
resetForm code = renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 2) (ColMd 0) (ColMd 4)) $ FormData
    <$> areq textField (bfs' "Reset code") ( Just code)
    <*> areq identityField (bfs' "Username") Nothing
    <*> areq passwordConfirmField (bfs' "Password") Nothing
    <*  bootstrapSubmit (BootstrapSubmit ("Submit"::Text) "btn-primary" [])

generateHtml :: Text -> Widget -> Enctype -> [Text] -> LHandler Html
generateHtml code widget enctype msg = 
    let title = generatePageTitle Nothing "Password Reset" in
    defaultLayout $
        let msgH = mconcat $ map displayError msg in
        do
        setTitle $ toHtml title
        [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Password Reset
                    ^{msgH}
                    <form method=post action="@{PasswordResetR code}" enctype=#{enctype} class="form-horizontal" role="form">
                        ^{widget}
        |]

getPasswordResetR :: Text -> Handler Html
getPasswordResetR code = runLHandler $ do
    ( widget, enctype) <- generateFormPost $ resetForm code
    generateHtml code widget enctype []

postPasswordResetR :: Text -> Handler Html
postPasswordResetR code = runLHandler $ do
    ((res, widget), enctype) <- runFormPost $ resetForm code
    case res of
        FormFailure _msg ->
            generateHtml code widget enctype [] -- msg
        FormMissing ->
            generateHtml code widget enctype []
        FormSuccess (FormData code' username password) -> do
            mErr <- resetPassword code' username password
            case mErr of
                Nothing -> do
                    -- TODO: set message
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Successfully reset password!
                    |]
                    redirect $ AuthR LoginR
                Just err ->
                    generateHtml code widget enctype [err]
    
        
