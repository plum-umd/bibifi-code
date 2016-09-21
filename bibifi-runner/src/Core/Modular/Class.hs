{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Core.Modular.Class where

import Control.Monad.Error as E
import Cloud
import Core (keyToInt)
import Core.DatabaseM
import Data.Aeson (FromJSON(..), (.:), Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Database.Persist
import Model
import qualified Network.HTTP.Conduit as HTTP
import PostDependencyType
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

data RunnerOptions = RunnerOptions {
        runnerRepositoryPath :: FilePath
      , runnerCloudConfiguration :: CloudConfiguration
      , runnerHttpManager :: HTTP.Manager
      , runnerOracleDirectory :: FilePath
    }

class ModularContest contest where
    scoreContestBuild :: contest -> RunnerOptions -> DatabaseM ()
    scoreContestBreak :: contest -> RunnerOptions -> DatabaseM ()
    scoreContestFix :: contest -> RunnerOptions -> DatabaseM ()
    runOracleSubmission :: contest -> RunnerOptions -> Entity OracleSubmission -> DatabaseM (Maybe Bool) -- Return Nothing means timeout. True means success.
    runBuildSubmission :: contest -> RunnerOptions -> Entity BuildSubmission -> DatabaseM (Maybe (Bool, Bool)) -- Return Nothing means timeout. True means success, rescore.
    runBreakSubmission :: contest -> RunnerOptions -> Entity BreakSubmission -> DatabaseM (Maybe (Bool, Bool)) -- Return Nothing means timeout. True means success, rescore.
    runFixSubmission :: contest -> RunnerOptions -> Entity FixSubmission -> DatabaseM (Maybe (Bool, Bool)) -- Return Nothing means timeout. True means success, rescore.

getArchiveLocation :: (MonadIO m, E.Error e) => String -> String -> RunnerOptions -> ErrorT e m FilePath
getArchiveLocation team hash opts = ErrorT $ do
    let repoDir = runnerRepositoryPath opts
    let loc'' = FilePath.joinPath [repoDir, "archives", team, hash]
    let loc' = FilePath.addExtension loc'' "tar"
    let loc =  FilePath.addExtension loc' "gz"
    exists <- liftIO $ Directory.doesFileExist loc
    if exists then
        return $ Right loc
    else
        return $ Left $ E.strMsg $ "Archive does not exist: " <> loc

getBuildArchiveLocation :: (MonadIO m, E.Error e) => BuildSubmission -> RunnerOptions -> ErrorT e m FilePath
getBuildArchiveLocation submission opts = 
    let hash = Text.unpack $ buildSubmissionCommitHash submission in
    let team = show $ keyToInt $ buildSubmissionTeam submission in
    getArchiveLocation team hash opts

getBreakArchiveLocation :: (MonadIO m, E.Error e) => BreakSubmission -> RunnerOptions -> ErrorT e m FilePath
getBreakArchiveLocation submission opts = do
    -- Old for art gallery:
    -- let hash = Text.unpack $ breakSubmissionCommitHash submission in
    -- let team = show $ keyToInt $ breakSubmissionTeam submission
    -- getArchiveLocation team hash opts
    let team = show $ keyToInt $ breakSubmissionTeam submission
    let breakName = Text.unpack $ breakSubmissionName submission
    let repoDir = runnerRepositoryPath opts
    let loc' = FilePath.joinPath [repoDir, "breaks", team, breakName]
    let loc = FilePath.addExtension loc' "zip"
    exists <- liftIO $ Directory.doesFileExist loc
    if exists then
        return loc
    else
        throwError $ E.strMsg $ "Archive does not exist: " <> loc

getFixArchiveLocation :: (MonadIO m, E.Error e) => FixSubmission -> RunnerOptions -> ErrorT e m FilePath
getFixArchiveLocation submission opts = 
    let hash = Text.unpack $ fixSubmissionCommitHash submission in
    let team = show $ keyToInt $ fixSubmissionTeam submission in
    getArchiveLocation team hash opts

data OracleOutput = 
      OracleOutputError Text
    | OracleOutputSuccess Text

instance FromJSON OracleOutput where
    parseJSON (Object o) = do
        result <- o .: "result"
        if result then do
            (output' :: Value) <- o .: "output"
            let prettyConf = Aeson.defConfig {Aeson.confIndent = 2}
            let output = Text.decodeUtf8With Text.lenientDecode $ BSL.toStrict $ Aeson.encodePretty' prettyConf output'
            return $ OracleOutputSuccess output
        else do
            err <- o .: "error"
            return $ OracleOutputError err
    parseJSON _ = mzero

-- data BuildError = 
--       BuildErrorInfrastructure String
--     | BuildErrorBuildFailed

parseTestHelper :: (FromJSON i, Monad m, E.Error e) => (t -> Text) -> (Entity t -> a) -> Entity t -> ErrorT e m (a, i)
parseTestHelper getInput constr t@(Entity _ test) = ErrorT $ 
    let input' = getInput test in
    case Aeson.decodeStrict' $ Text.encodeUtf8 input' of
        Nothing ->
            return $ Left $ E.strMsg $ "Invalid test input: " <> (show input')
        Just input ->
            return $ Right $ (constr t, input)

class ModularBreakTest b where
    breakTestToType :: b -> BreakType
