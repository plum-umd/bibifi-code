{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cloud
import Control.Concurrent
import Control.Monad
-- import Core.Modular
import qualified Data.Set as Set
import qualified Network.HTTP.Conduit as HTTP
import System.Posix.Signals

import Common
import Options
import qualified Queue
import Runner
import Problem
import Scorer
import Scheduler

main :: IO ()
main = do
    -- Read EC2 configuration file. 
    ec2 <- loadCloudConfiguration "../config/cloud.yml" -- productionCloudYML

    -- Create database pool and config.
    db <- makeDatabaseConf productionDatabaseYML "Production" -- "config/postgresql.yml"

    -- Read command line arguments. 
    (Options count fsDir oracleDir contest) <- runDatabaseM db parseOptions
    
    -- Create exiting mvar. 
    exiting <- newEmptyMVar

    -- Create queue.
    queue <- newMVar $ Queue.empty

    -- Create blocked teams set.
    blockedTeams <- newMVar $ Set.empty

    -- Handle interupts and kills. 
    _ <- installHandler sigINT (sigHandler exiting count) Nothing

    -- Set runner options.
    http <- liftIO $ cloudManagerSettings ec2 >>= HTTP.newManager
    let runnerOptions = RunnerOptions fsDir ec2 http oracleDir
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
        sigHandler exiting count = Catch $ do
            -- Change the handlers since we are now exiting. 
            _ <- installHandler sigINT sigHandlerExiting Nothing

            -- Print that we are terminating.
            putLog "runner: Will exit once all threads are finished."

            -- Set the MVar to the number of threads. 
            putMVar exiting (count + 1)

        sigHandlerExiting = Catch $ do
            putLog "runner: Already exiting."

