module Handler.Profile.Account.Edit (getProfileAccountEditR, postProfileAccountEditR) where

import Forms
import Forms.Survey
import Import
import qualified Profile
import Yesod.Form.Bootstrap3

profileForm :: (Route App -> [(Text,Text)] -> Text) -> Maybe SurveyFormData -> Form SurveyFormData
profileForm renderer form = renderBootstrap3 BootstrapBasicForm $ 
    surveyForm renderer form
    <*  bootstrapSubmit (BootstrapSubmit ("Update"::Text) "btn-primary" [])

makeForm :: (Route App -> [(Text,Text)] -> Text) -> UserId -> LHandler (Maybe (Key UserInformation), Form SurveyFormData)
makeForm renderer userId = do
    userInfoM <- runDB $ getBy $ UniqueUserInformation userId
    return ( entityKey <$> userInfoM, profileForm renderer $ (userInformationToSurveyForm . entityVal) <$> userInfoM)


generateHtml :: Widget -> Enctype -> [Text] -> LHandler Html
generateHtml widget enc msg = Profile.layout Profile.Account $ \uId -> do
    -- Set title. 
    lLift $ setTitle $ toHtml $ generatePageTitle Nothing "Edit Profile"

    let msgH = mconcat $ map displayError msg
    [whamlet|
        <a href="@{ProfileAccountR}" type=button .btn .btn-primary>
            Back
        <h1>
            Edit profile
        ^{msgH}
        <form method=post action=@{ProfileAccountEditR} enctype=#{enc} class="form-basic" role="form">
            ^{widget}
    |]

getProfileAccountEditR :: Handler Html
getProfileAccountEditR = runLHandler $ do
    uId <- requireAuthId

    renderer <- getUrlRenderParams
    (_, form) <- makeForm renderer uId
    ( widget, enctype) <- generateFormPost form

    generateHtml widget enctype []

postProfileAccountEditR :: Handler Html
postProfileAccountEditR = runLHandler $ do
    uId <- requireAuthId

    renderer <- getUrlRenderParams
    ( userInfoKeyM, form) <- makeForm renderer uId
    ((res, widget), enctype) <- runFormPost form

    case res of
        FormFailure _msg ->
            failed widget enctype
        FormMissing ->
            failed widget enctype
        FormSuccess dat -> do
            let up = maybe insert_ repsert userInfoKeyM
            runDB $ up $ surveyFormToUserInformation uId dat
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Successfully updated profile.
            |]
            redirect ProfileAccountEditR

    where
        failed widget enctype = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not update profile.
            |]
            generateHtml widget enctype []
        
