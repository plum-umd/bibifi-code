{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Core.Modular.Class where

-- import Control.Monad.Error as E
import Cloud
-- import Core (keyToInt)
import Core.DatabaseM
-- import Data.Aeson (FromJSON(..), (.:), Value(..))
-- import qualified Data.Aeson as Aeson
-- import qualified Data.Aeson.Encode.Pretty as Aeson
-- import qualified Data.ByteString.Lazy as BSL
-- import Data.Monoid
-- import Data.Text (Text)
-- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as Text
-- import qualified Data.Text.Encoding.Error as Text
import Database.Persist
import Model
import qualified Network.HTTP.Conduit as HTTP
-- import PostDependencyType
-- import qualified System.Directory as Directory
-- import qualified System.FilePath as FilePath

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
