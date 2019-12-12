module Problem.Class where

import Cloud
import Core.DatabaseM
import Database.Persist
import Model
import qualified Network.HTTP.Conduit as HTTP

data RunnerOptions = RunnerOptions {
        runnerRepositoryPath :: FilePath
      , runnerCloudConfiguration :: CloudConfiguration
      , runnerHttpManager :: HTTP.Manager
      , runnerProblemDirectory :: FilePath
    }

class ExtractContest a where
    extractContest :: a -> Entity Contest

extractContestId :: ExtractContest a => a -> ContestId
extractContestId = entityKey . extractContest

class ProblemRunnerClass runner where

    runOracleSubmission :: runner -> RunnerOptions -> Entity OracleSubmission -> DatabaseM (Maybe Bool) -- Return Nothing means timeout. True means success.

    runBuildSubmission :: runner -> RunnerOptions -> Entity BuildSubmission -> DatabaseM (Maybe (Bool, Bool)) -- Return Nothing means timeout. True means success, rescore.

    runBreakSubmission :: runner -> RunnerOptions -> Entity BreakSubmission -> Entity BreakFixSubmission -> DatabaseM (Maybe (Bool, Bool)) -- Return Nothing means timeout. True means success, rescore.
    
    runFixSubmission :: runner -> RunnerOptions -> Entity FixSubmission -> DatabaseM (Maybe (Bool, Bool)) -- Return Nothing means timeout. True means success, rescore.


