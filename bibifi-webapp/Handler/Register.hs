module Handler.Register (getRegisterR, postRegisterR) where

import Import as I hiding (renderBootstrap3)
import Control.Applicative
-- import Text.Hamlet (shamlet)
import Yesod.Auth.HashDB (setPassword)
import Yesod.Auth.OAuth2
import Forms
import Forms.Survey
import Yesod.Form.Bootstrap3
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text as T
-- import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html.Renderer.String as S (renderHtml)
import Network.Mail.Mime

data FormData = FormData {
    formIdent :: Text,
    formPassword :: Text,
    formEmail :: Text,
    formSurvey :: SurveyFormData
    }

userForm :: (Route App -> [(Text,Text)] -> Text) -> Form FormData
userForm render = do
    renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 6) (ColMd 0) (ColMd 4)) $ FormData
        <$> areq identityField (bfs' "Username") Nothing
        <*> areq passwordConfirmField (bfs' "Password") Nothing
        <*> areq emailField (bfs' "Email") Nothing
        <*> surveyForm render Nothing
        <* bootstrapSubmit (BootstrapSubmit ("Register"::Text) "btn-primary" [])
    
-- userForm :: Form User
-- userForm = renderDivs $ User -- renderBootstrap
--     <$> areq identityField "Username" Nothing
--     <*> areq passwordConfirmField "Password" Nothing
--     <*> pure ("salt"::Text)
--     <*> areq emailField "Email" Nothing
--     <*> lift (liftIO getCurrentTime)
--     <*> pure False

getUser :: FormData -> LHandler User
getUser dat = do
    now <- getCurrentTime
    isAdmin <- fmap (== 0) $ runDB $ count ([] :: [Filter User])
    return $ User 
        (formIdent dat)
        (formPassword dat)
        "salt"
        (formEmail dat)
        now
        isAdmin
        Nothing
        Nothing

storeInformation :: UserId -> FormData -> LHandler ()
storeInformation uId dat = do
    let info = surveyFormToUserInformation uId $ formSurvey dat
    runDB $ insert_ info
    return ()

checkDeadline :: LHandler ()
checkDeadline = do
    return ()
    -- duringDeadline <- do
    --     contestM <- retrieveContest Nothing
    --     case contestM of 
    --         Nothing ->
    --             return False
    --         Just (Entity _ contest) -> do
    --             now <- lift getCurrentTime
    --             -- Set deadline to 1 hours before build-it round.
    --             let deadlineStart = addUTCTime (-1*60*60) $ contestBuildStart contest
    --             --let deadlineStart = addUTCTime (-2*24*60*60) $ contestBuildStart contest
    --             let deadlineEnd = contestFixEnd contest
    --             return $ now > deadlineStart && now < deadlineEnd
    -- when duringDeadline $ do
    --     setMessage [shamlet|
    --         <div .container>
    --             <div .alert .alert-warning>
    --                 Sorry, the deadline to register for this contest has already passed.
    --     |]
    --     redirect AnnouncementsR
                
-- requireCourseraCreds :: LHandler Text
-- requireCourseraCreds = do
--     cIdM <- lookupSession courseraSessionKey
--     case cIdM of
--         Nothing ->
--             -- Redirect to coursera login.
--             redirect $ AuthR $ oauth2Url "coursera"
--         Just courseraId ->
--             return courseraId

storeCourseraInfo :: UserId -> Widget -> Enctype -> LHandler Html -> LHandler Html
storeCourseraInfo uId widget enctype cps = do
    cIdM <- lookupSession courseraSessionKey
    tokenM <- lookupSession courseraSessionTokenKey
    case (cIdM, tokenM) of
        (Just courseraId, Just token) -> do
            cIdM <- runDB $ insertUnique $ CourseraUser courseraId uId token
            case cIdM of
                Nothing -> do
                    -- On fail, delete user record. 
                    runDB $ delete uId
                    -- Display error message. 
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-warning">
                                Coursera account is already in use, or user account is already associated with a Coursera account. 
                    |]
                    generateHtml widget enctype []
                Just _ -> do
                    deleteSession courseraSessionKey
                    deleteSession courseraSessionTokenKey
                    cps
        _ ->
            cps

generateHtml :: Widget -> Enctype -> [Text] -> LHandler Html
generateHtml widget enctype msg = 
    let title = generatePageTitle Nothing "Register" in
    defaultLayout $ do
        lLift $ setTitle $ toHtml title
        -- Check if user is logged in.
        muId <- handlerToWidget $ maybeAuthId
        case muId of
            Nothing -> do
                handlerToWidget $ checkDeadline
                let msgH = mconcat $ map displayError msg
                [whamlet|$newline never
                    <div class="row">
                        <div class="col-md-12">
                            <div class="page-header">
                                <h1>
                                    Register
                            ^{msgH}
                            <form method=post action=@{RegisterR} enctype=#{enctype} class="form-horizontal" role="form">
                                ^{widget}
                |]
            Just _ ->
                [whamlet|$newline never
                    <div class="row">
                        <div class="col-md-12">
                            <div class="page-header">
                                <h1>
                                    Register
                            You already have an account!
                |]
                
getRegisterR :: Handler Html
getRegisterR = runLHandler $ do
    render <- getUrlRenderParams
    ( widget, enctype) <- generateFormPost $ userForm render
    generateHtml widget enctype []

sendConfirmation :: Text -> Text -> LHandler ()
sendConfirmation email code = do
    renderer <- lLift getUrlRenderParams
    url <- return $ T.pack $ S.renderHtml $ [hamlet|@{ConfirmationR code}|] renderer
    to <- return [Address Nothing email]
    head <- return [("Subject", "Verify your account")]
    text <- return $ Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Please confirm your account by clicking on the link below.

\#{url}

Thanks!
|]
    textPart <- return $ Part { 
        partType = "text/plain; charset=utf-8",
        partEncoding = None,
        partDisposition = DefaultDisposition,
        partContent = PartContent text,
        partHeaders = []
    }
    html <- return $ renderHtml [shamlet|
        <p>Please confirm your email address by clicking on the link below.
        <p>
            <a href=#{url}>#{url}
        <p>Thanks!
    |]
    htmlPart <- return $ Part { 
        partType = "text/html; charset=utf-8",
        partEncoding = None,
        partDisposition = DefaultDisposition,
        partContent = PartContent html,
        partHeaders = []
    }
    mail <- initEmptyMail
    liftIO $ renderSendMail mail
        { mailTo = to, mailHeaders = head, mailParts = [[textPart, htmlPart]] }

generateConfirmation :: UserId -> LHandler Text
generateConfirmation userId = do
    code <- liftIO $ I.randomString 20
    res <- runDB $ insertUnique $ UserConfirmation userId code
    case res of
        Nothing ->
            generateConfirmation userId
        Just _ ->
            return code

postRegisterR :: Handler Html
postRegisterR = runLHandler $ do
    checkDeadline
    render <- getUrlRenderParams
    ((res, widget), enctype) <- runFormPost $ userForm render
    action <- lLift $ getIPAddress >>= (return . RateLimitRegister)
    actionAllowed <- lLift $ canPerformAction action
    if not actionAllowed then do
        setMessage [shamlet|
            <div class="container">
                <div class="alert alert-warning">
                    Sorry, you cannot do that at this time. Please try again later. 
        |]
        generateHtml widget enctype []
    else do
        case res of
            FormSuccess dat ->
                -- Check that confirmation is true.
                -- Changed for coursera: if (formConfirmation dat) then do
                if True then do
                    user <- getUser dat
                    u <- setPassword (userPassword user) user
                    res' <- runDB $ insertUnique u
                    case res' of 
                        Nothing ->
                            generateHtml widget enctype ["Sorry, that username or email already exists."]
                        Just uId -> do
                            -- Store coursera info.
                            storeCourseraInfo uId widget enctype $ do
                                -- Store user information
                                storeInformation uId dat

                                -- Create and send confirmation code.
                                code <- generateConfirmation uId
                                sendConfirmation (userEmail u) code

                                -- Log rate limited action. 
                                lLift $ recordAction action

                                -- Set message and redirect.
                                setMessage [shamlet|$newline never
                                    <div class="container">
                                        <div class="alert alert-success">
                                            Successfully created an account! Please check your inbox to verify your email. 
                                            <strong>
                                                Be sure to either join or create a team!
                                |]
                                redirectUltDest ContestSignupR
                else
                    generateHtml widget enctype ["You must accept the agreement."]
            FormFailure _msg ->
                generateHtml widget enctype [] -- msg
            FormMissing ->
                generateHtml widget enctype []

