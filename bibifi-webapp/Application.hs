{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.Persist.Sql (runMigration) -- (runMigration, printMigration)
import Network.HTTP.Client.Conduit (newManager)
import Data.Default (def)
import Control.Monad.Logger (runLoggingT)
import Control.Concurrent (forkIO, threadDelay)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import qualified Network.URI as URI
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.ForceDomain
import Yesod.Core.Types (loggerSet, Logger (Logger))

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Admin
import Handler.Admin.Contests
import Handler.Admin.Contest
import Handler.Admin.Contest.BuildSubmissions
import Handler.Admin.Contest.BreakSubmissions
import Handler.Admin.Contest.FixSubmissions
import Handler.Admin.Contest.Create
import Handler.Admin.Contest.Edit
import Handler.Admin.Contest.MakeJudge
import Handler.Admin.Contest.JudgeEmails
import Handler.Admin.Contest.Judgements
import Handler.Admin.Contest.Judgements.AssignBuild
import Handler.Admin.Contest.Judgements.AssignBreak
import Handler.Admin.Contest.Judgements.AssignFix
import Handler.Admin.Contest.ParticipantEmails
import Handler.Admin.Contest.MakeDefault
import Handler.Admin.Contest.OracleBreaks
import Handler.Admin.Contest.OracleBreaks.Create
import Handler.Admin.Contest.OracleBreaks.Edit
import Handler.Admin.Contest.Teams
import Handler.Admin.Contest.Tests
import Handler.Admin.Contest.Tests.CreateCorrectness
import Handler.Admin.Contest.Tests.CreateOptional
import Handler.Admin.Contest.Tests.CreatePerformance
import Handler.Admin.Contest.Tests.Correctness
import Handler.Admin.Contest.Tests.Optional
import Handler.Admin.Contest.Tests.Performance
-- import Handler.Admin.Contest.OracleBreaks.Edit
import Handler.Admin.Team
import Handler.Admin.Team.AddMember
import Handler.Admin.Team.Member
import Handler.Admin.Teams
import Handler.Admin.Users
import Handler.Admin.User
import Handler.Admin.User.ResetPassword
import Handler.Admin.User.SetAdmin
import Handler.Announcements
import Handler.Scoreboard
import Handler.PerformanceRankings
import Handler.Contests
import Handler.Contest.Participation
import Handler.ContestSignup
import Handler.Confirmation
import Handler.CreateTeam
import Handler.Details
import Handler.DownloadFile
import Handler.Feedback
import Handler.Judges
import Handler.Judges.BuildIt
import Handler.Judges.BreakIt
import Handler.Judges.FixIt
import Handler.Participation
import Handler.Participation.Information
import Handler.Participation.OracleSubmissions
import Handler.Participation.OracleSubmissions.Create
import Handler.Participation.BuildSubmissions
import Handler.Participation.BuildersCode
import Handler.Participation.BreakSubmissions
import Handler.Participation.FixSubmissions
import Handler.PasswordReset
import Handler.Profile
import Handler.Profile.Account
import Handler.Profile.Account.Edit
import Handler.Profile.Account.Upload
import Handler.Profile.Teams
import Handler.Register
import Handler.Splash
import Handler.Sponsorship
import Handler.Support
import Handler.Team
import Handler.TeamInvitation hiding (error)
import Handler.Team.AddMembers
import Handler.Team.Information
import Handler.Team.Leave
import Handler.Team.Participation
import Handler.Thanks
import Handler.Splash
import Handler.Sponsorship
import Handler.Support
import Handler.Feedback
import Handler.Admin
import Handler.Admin.Announcements
import Handler.Admin.AddAnnouncement
import Handler.Admin.Contests
import Handler.Admin.Contest
import Handler.Admin.Contest.MakeDefault
import Handler.Todo
import Handler.Webhook.Gitlab.Push

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app'' <- toWaiAppPlain foundation
    let app' = logWare app''

    -- Force domain name.
    let domainName = Text.encodeUtf8 $ appDomainName foundation
    let app = if development then
            app'
          else
            forceDomain (\d' -> 
                let d = BSC.takeWhile (/= ':') d' in
                if d /= domainName then Just domainName else Nothing
              ) app'
    return app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager -- def
    s <- staticSite
    dbconf <- withYamlEnvironment "../config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    
    --logger <- mkLogger True stdout
    
    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher

    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    _ <- forkIO updateLoop

    -- Load git configuration.
    gitConfig <- loadGitConfiguration "../config/git.yml" >>= \case
        Left e -> error e
        Right c -> return c


    let logger = Yesod.Core.Types.Logger loggerSet' getter
    let foundation = App conf s p manager dbconf logger domainName gitConfig

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

    where
        domainName = case (URI.parseURI $ Text.unpack $ appRoot conf) >>= URI.uriAuthority of
            Nothing ->
                error $ "Could not parse domain name: " <> Text.unpack (appRoot conf)
            Just uri ->
                Text.pack $ URI.uriRegName uri

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        , csFile = \_ -> return "../config/settings.yml"
        }
