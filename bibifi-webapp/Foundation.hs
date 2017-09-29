{-# LANGUAGE ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Foundation where

import Prelude
import Database.LPersist as LP
import Core.Database
import LMonad
import LMonad.Label.DisjunctionCategory
import LMonad.Yesod
import Yesod as Y hiding (widgetToPageContent, whamlet)
import qualified Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB, validateUser)
--import Yesod.Auth.Email
--import Yesod.Auth.BrowserId
--import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Settings.StaticFiles
import Settings (Extra (..))
import Model
import Control.Applicative
import Control.Monad (unless)
import Data.Text
import Data.Maybe (fromMaybe)
import Data.Monoid
import Text.Jasmine (minifym)
import Text.Lucius
-- import System.Log.FastLogger (Logger)
import Yesod.Core.Types (Logger)
-- import qualified Admin

import Contest
import Database.Persist.RateLimit
import RateLimit
import Yesod.Auth.OAuth2.Coursera



-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "../config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Define css layout.
css :: CssUrl url
css = 
    let footerHeight = 20 :: PixelSize in
    let footerMargin = 40 :: PixelSize in
    let footerBottomMargin = 4 :: PixelSize in
    -- let textColor = Color 255 255 255 in
    [lucius|
        html, body {
            height: 100%;
        }
        
        .wrapper {
            min-height: 100%;
            height: auto !important;
            height: 100%;
            margin: 0 auto -#{footerBottomMargin + footerHeight};
            padding: 10px 0px 0px;
        }

        div.splash {
            font-size: 22px;
        }
        
        .push {
            height: #{footerMargin + footerHeight};
        }
        
        footer {
            height: #{footerHeight};
            text-align: center;

            * {
                margin: 0px;
                padding: 0px;
            }

            ul {
                list-style: none;
            }

            li {
                display: inline-block;
                margin: 0px 4px;
            }

            .seperator {
                border-left: 1px solid #ddd;
                width: 1px;
                height: 14px;
                margin-bottom: -2px;
            }
        }
        
        .last {
            margin-bottom: 0px;
        }
        
        .clear {
            clear: both;
        }
        
        .navbar {
            margin: 0px;
            padding: 0px;
            background: transparent !important;
            -webkit-box-shadow: 0px 0px;
            box-shadow: 0px 0px;
        
            .navbar-header {
                img {
                    width: 100%;
                }
            }
        
            .seperator {
                border-left: 1px solid #ddd;
                width: 1px;
                margin: 12px 10px 8px 10px;
                height: 14px;
            }
        
            .navbar-toggle {
                border-color: #ccc;
            }
        
            .icon-bar {
                background-color: #ddd;
            }

            .linksbar {
                margin-top: 10px;
                margin-bottom: 10px;
            }
        }

        .langdon {
            font-family: "langdon", 'Helvetica Neue', Helvetica, Arial, sans-serif;
        }
    |]

-- Layout for navigation bar.
-- TODO: check current path and site..
navbar :: LayoutData -> Handler (HtmlUrl (Route App))
navbar contest = do
    mauth <- maybeAuth
    participantLinks <- participantNav contest mauth
    contestLinks <- case contest of
        Just (Entity _ c) ->
            let url = contestUrl c in
            return [hamlet|
                <li>
                    <a href=@{SpecificAnnouncementsR url}>
                        ANNOUNCEMENTS
                <li>
                    <a href=@{SpecificScoreboardR url}>
                        SCOREBOARD
            |]
        Nothing -> return [hamlet|
            <li>
                <a href=@{AnnouncementsR}>
                    ANNOUNCEMENTS
            <li>
                <a href=@{ScoreboardR}>
                    SCOREBOARD
        |]
    accountLinks <- case mauth of
        Just (Entity _ u) ->
            let adminNav =
                  if userAdmin u then
                    [hamlet|
                        <li>
                            <a href=@{AdminR}>
                                ADMIN
                    |]
                  else
                    mempty
            in
            return [hamlet|
                ^{adminNav}
                <li>
                    <a href=@{ProfileR}>
                        PROFILE
                <li>
                    <a href=@{AuthR LogoutR}>
                        LOGOUT
            |]
        Nothing -> return [hamlet|
            <li>
                <a href=@{AuthR LoginR}>
                    LOGIN
            <li>
                <a href=@{RegisterR}>
                    REGISTER
        |]
            
    return [hamlet|
        <div class="navbar" role="navigation">
            <div class="container">
                <div class="collapse navbar-collapse navbar-ex1-collapse linksbar">
                    <ul class="nav nav-pills navbar-right">
                        <li>
                            <a href=@{SponsorshipR}>
                                SPONSORSHIP OPPORTUNITIES
                        ^{participantLinks}
                        ^{contestLinks}
                        <li>
                            <a href=@{DetailsR}>
                                DETAILS
                        <li class="seperator">
                        ^{accountLinks}
                <div class="navbar-header">
                    <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-ex1-collapse">
                         <span class="sr-only">
                             Toggle navigation
                         <span class="icon-bar">
                         <span class="icon-bar">
                         <span class="icon-bar">
                    <a class="brand" href="/">
                        <img src=@{StaticR img_builditbreakitfixit_svg}>
    |]

    where
        -- If logged in and participating in the contest, link to ContestParticipationR url
        participantNav contest Nothing = return mempty
        participantNav Nothing u = do
            contestM <- defaultContest
            case contestM of
                Nothing -> return mempty
                Just _ -> participantNav contestM u
        participantNav (Just (Entity contestId contest)) (Just (Entity userId _)) = do
            signedUp <- userIsSignedupForContest userId contestId

            if signedUp then
                return [hamlet|
                    <li>
                        <a href="@{ContestParticipationR $ contestUrl contest}">
                            PARTICIPANTS
                |]
            else
                return mempty

-- Custom layout.
type LayoutData = Maybe (Entity Contest)
--
--
-- customLayout :: LayoutData -> LMonadT (DCLabel Principal) (WidgetT App IO) () -> LMonadT (DCLabel Principal) (HandlerT App IO) Html
-- 
-- widgetToPageContent :: LWidget () -> LHandler Html
--
-- handlerToWidget :: LWidget a -> LHandler a
--
--
--

defaultLayout :: LMonadT (DCLabel Principal) (WidgetT App IO) () -> LMonadT (DCLabel Principal) (HandlerT App IO) Html
defaultLayout = customLayout Nothing

customLayout :: LayoutData -> LMonadT (DCLabel Principal) (WidgetT App IO) () -> LMonadT (DCLabel Principal) (HandlerT App IO) Html
customLayout d widget = do
        -- master <- getYesod
        -- mmsg <- getMessage

        nav <- lLift $ navbar d

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        mmsg <- lLift getMessage
        let msg = case mmsg of
              Nothing ->
                mempty
              Just h ->
                h

        -- pc <- widgetToPageContent widget
            -- widget -- $(widgetFile "default-layout") -- $(hamletFile "templates/default-layout-wrapper.hamlet")
        layout <- widgetToPageContent $ do
            lLift $ do
    --            addStylesheet $ StaticR css_bootstrap_min_css
                $(combineStylesheets 'StaticR
                    [ css_bootstrap_min_css
                    , css_langdon_css
                    , css_jquery_ui_css
                    , css_bootstrap_tokenfield_min_css
                    , css_jquery_datetimepicker_min_css
                    -- , css_bootstrap_theme_min_css
                    ])
                (toWidget css)
    --            addScript $ StaticR js_jquery_min_js
    --            addScript $ StaticR js_bootstrap_min_js
                $(combineScripts 'StaticR
                    [ js_jquery_min_js
                    , js_bootstrap_min_js
                    , js_jquery_ui_min_js
                    , js_jquery_ui_min_js
                    , js_jquery_datetimepicker_full_min_js
                    , js_bootstrap_tokenfield_js
                    -- , js_jquery_validate_min_js
                    ])
                -- Analytics code.
                unless development $ toWidget [julius|
                        (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                        })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
    
                        ga('create', 'UA-52699115-1', 'auto');
                        ga('send', 'pageview');
                    |]
            widget' <- extractWidget widget
            [whamlet|
                <div class="wrapper">
                    <noscript>
                        <div class="container">
                            <div class="alert alert-warning">
                                <strong>Warning!</strong> This web site requires javascript.
                    #{msg}
                    ^{nav}
                    <div id="main" role="main">
                        <div class="container">
                            ^{widget'}
                    <div class="push container">
                <footer>
                    <div class="container">
                        <ul>
                            <li>
                                <a href=@{SupportR}>
                                    Support
                            <li class="seperator">
                            <li>
                                <a href=@{FeedbackR}>
                                    Feedback
                            <li class="seperator">
                            <li>
                                <a href=@{ContestsR}>
                                    Past Contests
                \<!-- Prompt IE 6 users to install Chrome Frame. Remove this if you want to support IE 6.  chromium.org/developers/how-tos/chrome-frame-getting-started -->
                \<!--[if lt IE 7 ]>
                    <script src="http://ajax.googleapis.com/ajax/libs/chrome-frame/1.0.3/CFInstall.min.js">
                    <script>
                        window.attachEvent('onload',function(){CFInstall.check({mode:'overlay'})})
                \<![endif]-->
            |]
            
        lLift $ giveUrlRenderer [hamlet|$newline never
\<!doctype html>
\<!--[if lt IE 7]> <html class="no-js ie6 oldie" lang="en"> <![endif]-->
\<!--[if IE 7]>    <html class="no-js ie7 oldie" lang="en"> <![endif]-->
\<!--[if IE 8]>    <html class="no-js ie8 oldie" lang="en"> <![endif]-->
\<!--[if gt IE 8]><!-->
<html class="no-js" lang="en"> <!--<![endif]-->
    <head>
        <meta charset="UTF-8">

        <title>#{pageTitle layout}
        <meta name="description" content="The Build It Break It Fix It programming contest challenges participants to develop secure, but efficient software.">
        <meta name="author" content="UMD, Department of Computer Science, PLUM, James Parker, Andrew Ruef, Michael Hicks">
        <meta name="viewport" content="width=device-width,initial-scale=1">

        ^{pageHead layout}

        \<!--[if lt IE 9]>
        \<script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
        \<![endif]-->

        <script>
          document.documentElement.className = document.documentElement.className.replace(/\bno-js\b/,'js');

    <body>
        ^{pageBody layout}
|]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "../config/client_session_key.aes"

    defaultLayout = runLHandler . customLayout Nothing . lLift

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
--    addStaticContent a b c = do
--        liftIO $ putStr "****** here ******\n"
--        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute []) a b c
    addStaticContent = do
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    maximumContentLength _ (Just ProfileAccountConsentR) = Just $ 4 * 1024 * 1024 -- 4 megabytes
    maximumContentLength _ (Just ProfileAccountResumeR) = Just $ 4 * 1024 * 1024 -- 4 megabytes
    maximumContentLength _ _ = Just $ 2 * 1024 * 1024 -- 2 megabytes

 -- -- Handle email authentication.
 -- instance YesodAuthEmail App where
 --     type AuthEmailId App = EmailId
 -- 
 --     addUnverified email verkey = runDB $ insert $
 --         Email email (Just verkey)
 -- 
 --     sendVerifyEmail email _ verurl = liftIO $ -- TODO log and email this
 --         putStrLn $ show verurl
 -- 
 --     getEmailCreds email = runDB $ do
 --         res <- getBy $ UniqueEmail email
 --         return $ case res of
 --             Nothing ->
 --                 Nothing
 --             Just e -> do
 --                 Nothing -- TODO!!
 --                 
 --     getEmail eid = runDB $ do
 --         res <- get eid
 --         return $ case res of
 --             Nothing ->
 --                 Nothing
 --             Just e ->
 --                 Just $ emailEmail e
 -- 
 --     getVerifyKey eid = runDB $ do
 --         res <- get eid
 --         return $ case res of 
 --             Nothing ->
 --                 Nothing
 --             Just e -> 
 --                 emailVerkey e
 -- 
 --     setVerifyKey eid key = runDB $
 --         update eid [EmailVerkey =. Just key]
 -- 
 --     verifyAccount eid = runDB $ do
 --         res <- getBy $ UniqueUserEmail eid
 --         case res of 
 --             Nothing ->
 --                 return Nothing
 --             Just (Entity uid _) -> do
 --                 res' <- get eid
 --                 case res' of 
 --                     Nothing ->
 --                         return Nothing
 --                     Just e -> do
 --                         update uid [UserVerified =. True]
 --                         return $ Just uid
 -- 
 -- --    afterPasswordRoute site = -- TODO set to contest/team registration?
 -- 

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = AnnouncementsR
    -- Where to send a user after logout
    logoutDest _ = AnnouncementsR

    getAuthId c@(Creds "hashdb" _ _ ) = do
        uIdM <- getAuthIdHashDB AuthR (Just . UniqueUser) c
        case uIdM of
            Nothing ->
                return Nothing
            Just uId -> do
                -- Check if there are coursera session variables.
                cIdM <- lookupSession courseraSessionKey
                tokenM <- lookupSession courseraSessionTokenKey
                case (cIdM, tokenM) of
                    (Just courseraId, Just token) -> do
                        _ <- Y.runDB $ Y.insertUnique $ CourseraUser courseraId uId token
                        return ()
                    _ ->
                        return ()
                deleteSession courseraSessionKey
                deleteSession courseraSessionTokenKey
                return uIdM

        --         -- For coursera, check that user has an associated coursera account. 
        --         userM <- Y.runDB $ Y.getBy $ UniqueCourseraUser uId
        --         case userM of
        --             Nothing ->
        --                 -- User doesn't have an associated coursera account so redirect there. 
        --                 
        --             Just _ ->
        --                 return uIdM
        
    getAuthId (Creds "coursera" cId extra) = do
        -- Check if already authenticated. 
        idM <- maybeAuthId
        userM <- Y.runDB $ Y.getBy $ UniqueCourseraId cId
        case idM of
            -- Logged in.
            Just uId -> do
                case userM of
                    -- User doesn't have a registered coursera account so insert it. 
                    Nothing -> 
                        case lookup "token" extra of
                            Nothing -> do
                                setMessage [shamlet|
                                    <div class="container">
                                        <div class="alert alert-warning">
                                            Could not retrieve Coursera token.
                                |]
                                return Nothing
                            Just token -> do
                                cIdM <- Y.runDB $ Y.insertUnique $ CourseraUser cId uId token
                                case cIdM of
                                    Nothing -> do
                                        -- Coursera account is associated with another account. 
                                        setMessage [shamlet|
                                            <div class="container">
                                                <div class="alert alert-warning">
                                                    Coursera account is already associated with another account. 
                                        |]
                                        return Nothing
                                    Just _ ->
                                        return idM
                    -- Has a coursera account so update the token.
                    Just (Entity cuId _) -> do
                        -- Update token.
                        updateToken cuId extra
                        return idM
            -- User not logged in.
            Nothing ->
                case userM of 
                    Nothing -> do
                        -- User isn't logged in so set session and redirect to registration.
                        token <- getToken extra
                        setSession courseraSessionKey cId
                        setSession courseraSessionTokenKey token
                        msg <- withUrlRenderer [hamlet|
                            <div class="container">
                                <div class="alert alert-success">
                                    Please create an account for the contest. If you already have an account, please <a href="@{AuthR LoginR}">login</a> with your password. 
                        |]
                        setMessage msg
                        redirect RegisterR
                    Just (Entity cuId cu) -> do
                        -- Update token.
                        updateToken cuId extra
                        return $ Just $ courseraUserUser cu
        where
            updateToken cuId extra = case lookup "token" extra of
                Nothing -> do
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-warning">
                                Could not retrieve Coursera token.
                    |]
                Just token -> do
                    Y.runDB $ Y.update cuId [CourseraUserToken =. token]

            getToken extra = case lookup "token" extra of
                Nothing -> do
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-warning">
                                Could not retrieve Coursera token.
                    |]
                    redirect $ AuthR LoginR
                Just token ->
                    return token
                

    getAuthId _ = notFound


--     getAuthId creds = runDB $ do
--         x <- getBy $ UniqueUser $ credsIdent creds
--         -- t <- liftIO getCurrentTime -- TODO: what is this supposed to do??
--         return $ case x of
--             Just (Entity uid _) ->
--                 Just uid
--             Nothing -> 
--                 Nothing


--            do
--                fmap Just $ insert $ User (credsIdent creds) Nothing t False -- TODO: right defaults?

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = 
        let link = PluginR "hashdb" ["login"] in
        let login tm = do
            toWidget [lucius|
                .strike {
                    display: block;
                    text-align: center;
                    overflow: hidden;
                    white-space: nowrap; 
                    margin-top: 20px;
                    margin-bottom: 20px;
                }

                .strike > span {
                    position: relative;
                    display: inline-block;
                }
                
                .strike > span:before,
                .strike > span:after {
                    content: "";
                    position: absolute;
                    top: 50%;
                    width: 9999px;
                    height: 1px;
                    background: #eee;
                }

                .strike > span:before {
                    right: 100%;
                    margin-right: 15px;
                }

                .strike > span:after {
                    left: 100%;
                    margin-left: 15px;
                }
            |]
            [Yesod.whamlet|
                <div class="row" style="margin-top: 20px">
                    <div class="col-md-4 col-md-offset-4">
                        <div class="panel panel-default">
                            <div class="panel-heading">
                                <h3 class="panel-title">
                                    Login
                            <div class="panel-body">
                                <a href="@{AuthR oauth2CourseraUrl}" class="btn btn-default btn-block" role="button">
                                    Login via Coursera
                                <div class="strike">
                                    <span>
                                        Or
                                <form method="post" action="@{tm link}" role="form">
                                    <div class="form-group">
                                        <label for="x">
                                            Username
                                        <input id="x" class="form-control" name="username" autofocus="" placeholder="Enter username" required>
                                    <div class="form-group">
                                        <label for="password">
                                            Password
                                        <input type="password" class="form-control" name="password" placeholder="Password" required>
                                    <button type="submit" class="btn btn-default">
                                        Login
                                    <a class="pull-right" style="padding: 7px 0px" href="@{RegisterR}">
                                        Create an account
                                <script>
                                    if (!("autofocus" in document.createElement("input"))) {
                                        document.getElementById("x").focus();
                                    }
            |]
        in
        [ AuthPlugin "hashdb" dispatch login
        , noLogin $ oauth2Coursera "2uJBiupA011CkZY4m_04aw" "eySvizRP9soTW1YcS-4f3Q" ["view_profile"]]

        where
            noLogin p = p {apLogin = mempty}

            dispatch "POST" ["login"] = postLoginR >>= sendResponse
            dispatch _ _              = notFound

            -- | Handle the login form. First parameter is function which maps
            --   username (whatever it might be) to unique user ID.
            postLoginR = do
                tm <- getRouteToParent
                action <- lift $ getIPAddress >>= (return . RateLimitLoginAttempt)
                allowed <- lift $ canPerformAction action
                lift $ recordAction action
                if not allowed then
                    lift $ loginErrorMessage (tm LoginR) "Sorry, you cannot do that at this time. Please try again later."
                else do
                    (mu,mp) <- lift $ runInputPost $ (,)
                        <$> iopt textField "username"
                        <*> iopt textField "password"
            
                    isValid <- lift $ fromMaybe (return False) 
                                 (validateUser <$> ((Just . UniqueUser) =<< mu) <*> mp)
                    lift $ if isValid then do
                        deleteRecordedAction action
                        setCredsRedirect $ Creds "hashdb" (fromMaybe "" mu) []
                    else do
                        loginErrorMessage (tm LoginR) "Invalid username/password"

    authHttpManager = httpManager

    onLogin = 
        setMessage [shamlet|
            <div class="container">
                <div class="alert alert-success">
                    You are now logged in.
        |]

    onLogout = 
        setMessage [shamlet|
            <div class="container">
                <div class="alert alert-success">
                    You are now logged out.
        |]

    onErrorHtml dest msg = do
        setMessage [shamlet|
            <div class="container">
                <div class="alert alert-danger">
                    #{msg}
        |]
        redirect dest
        
instance YesodAuthPersist App 

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email




--
-- Noninterference stuff. 
--

type LHandler = LMonadT (DCLabel Principal) Handler

instance LMonad Handler where
    lFail = permissionDenied "Sorry, you do not have permission to view this page."
    lAllowLift = return True
    
type LWidget = LMonadT (DCLabel Principal) (Yesod.WidgetT App IO) ()

instance LMonad (Yesod.WidgetT App IO) where
    lFail = Yesod.handlerToWidget lFail
    lAllowLift = Yesod.handlerToWidget lAllowLift

instance YesodLPersist App where
    runDB = lDefaultRunDB persistConfig connPool

runLHandler :: LHandler a -> Handler a
runLHandler = runLMonad

--
-- End noninterference stuff. 
--

courseraSessionKey :: Text
courseraSessionKey = "_courseraIdKey"

courseraSessionTokenKey :: Text
courseraSessionTokenKey = "_courseraTokenKey"

instance Label l => GeneralPersist App (LMonadT l (HandlerT App IO)) where
    type GeneralPersistBackend App = SqlBackend
    -- type GeneralPersistBackend App = YesodPersistBackend AppBackend
    runDB' = LP.runDB

instance Label l => GeneralPersistSql App (LMonadT l (HandlerT App IO))
