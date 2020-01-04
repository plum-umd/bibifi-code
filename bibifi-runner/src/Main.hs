{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cloud
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
-- import Core.Modular
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.HTTP.Conduit as HTTP
import System.Posix.Signals

import Common
import Git
import Options
import qualified Queue
import Runner
import Problem
import Problem.Shared
import Scorer
import Scheduler

main :: IO ()
main = do
    -- Read EC2 configuration file. 
    ec2 <- loadCloudConfiguration "../config/cloud.yml" -- productionCloudYML

    -- Read git configuration file.
    git <- loadGitConfiguration "../config/git.yml"

    -- Read command line arguments. 
    (Options count fsDir problemDir contest db) <- parseOptions productionDatabaseYML
    
    -- Create exiting mvar. 
    exiting <- newEmptyMVar

    -- Create queue.
    queue <- newMVar $ Queue.empty

    -- Create blocked teams set.
    blockedTeams <- newMVar $ Set.empty

    -- Create file lock set.
    fileLockSet <- LockSet <$> newMVar Map.empty

    -- Handle interupts and kills. 
    _ <- installHandler sigINT (sigHandler exiting count) Nothing

    -- Load build-it tests.
    buildTests <- runDatabaseM db $ loadBuildTests $ entityKey contest

    -- Set runner options.
    http <- liftIO $ cloudManagerSettings ec2 >>= HTTP.newManager
    let runnerOptions = RunnerOptions fsDir ec2 http problemDir buildTests git fileLockSet 
    let problemRunner = contestToProblemRunner contest
    let contestScorer = contestToScorer contest

    -- Fork scorer. 
    _ <- forkIO $ runDatabaseM db $ 
        scorerLoop exiting contestScorer runnerOptions

    -- Fork `count` runner threads. 
    replicateM_ count $ forkIO $ runDatabaseM db $ 
        runner problemRunner runnerOptions blockedTeams queue exiting 

    -- Run scheduler. 
    runDatabaseM db $ 
        scheduler queue exiting blockedTeams contest

    -- Exit when scheduler finishes. 
    putLog "runner: Exiting..."

    where
        -- Loads and parses all build tests into memory once. Performance tests are ordered by their primary keys.
        loadBuildTests contestId = do
            -- Retrieve tests from database.
            coreTests' <- runDB $ selectList [ContestCoreTestContest ==. contestId] []
            performanceTests' <- runDB $ selectList [ContestPerformanceTestContest ==. contestId] []
            optionalTests' <- runDB $ selectList [ContestOptionalTestContest ==. contestId] [Asc ContestOptionalTestId]

            testsE <- runErrorT $ do
                coreTests <- mapM parseCoreTest coreTests'
                performanceTests <- mapM parsePerformanceTest performanceTests'
                optionalTests <- mapM parseOptionalTest optionalTests'

                return $ BuildTests coreTests performanceTests optionalTests

            case testsE of
                Left e ->
                    exitWithError $ "Invalid build test: " <> e
                Right tests ->
                    return tests

        sigHandler exiting count = Catch $ do
            -- Change the handlers since we are now exiting. 
            _ <- installHandler sigINT sigHandlerExiting Nothing

            -- Print that we are terminating.
            putLog "runner: Will exit once all threads are finished."

            -- Set the MVar to the number of threads. 
            putMVar exiting (count + 1)

        sigHandlerExiting = Catch $ do
            putLog "runner: Already exiting."

