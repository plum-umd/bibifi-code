module Scorer where

import Core.Modular
import Control.Monad.Trans.Control

import Common

scorer :: MVar Int -> ModContest -> RunnerOptions -> DatabaseM ()
scorer exiting contest runnerOptions = checkExit exiting $ do
    -- Check for rescore.
    let contestId = getModContestId contest
    rescores <- runDB $ selectList [ScorePendingContest ==. contestId] []
    flip mapM_ rescores $ \(Entity pendingId pending) -> do
        case scorePendingRound pending of
            ContestRoundBuild ->
                scoreModContestBuild contest runnerOptions
            ContestRoundBreak ->
                scoreModContestBreak contest runnerOptions
            ContestRoundFix ->
                scoreModContestFix contest runnerOptions
        -- Delete the pending request. 
        runDB $ delete pendingId

-- Loop until we've been told to exit. 
checkExit :: (MonadIO m, MonadBaseControl IO m) => MVar Int -> m () -> m ()
checkExit exiting f = do
    exitM <- liftIO $ tryTakeMVar exiting
    case exitM of
        Nothing -> do
            -- Run computation
            runComputation () f

            -- Sleep 10 seconds and then try again.
            liftIO $ threadDelay 10000000
            checkExit exiting f
        Just c -> do
            liftIO $ putMVar exiting $ c - 1

scoreModContestBuild :: ModContest -> RunnerOptions -> DatabaseM ()
scoreModContestBuild (ATMContest contest) = scoreContestBuild contest
scoreModContestBuild (ArtContest contest) = scoreContestBuild contest

scoreModContestBreak :: ModContest -> RunnerOptions -> DatabaseM ()
scoreModContestBreak (ATMContest contest) = scoreContestBreak contest
scoreModContestBreak (ArtContest contest) = scoreContestBreak contest

scoreModContestFix :: ModContest -> RunnerOptions -> DatabaseM ()
scoreModContestFix (ATMContest contest) = scoreContestFix contest
scoreModContestFix (ArtContest contest) = scoreContestFix contest

