module Scorer where

import Control.Monad
import Control.Monad.Trans.Control
import Data.IORef
import Data.Time

import Common
import Problem.Class (ExtractContest(..), extractContestId, RunnerOptions(..))
import Scorer.Class

-- Import additional contest specification instances here.
import Problem.ATM (ATMSpec(..))
import Problem.ArtGallery (ArtGallery(..))
import Problem.EHR (EHRSpec(..))
import Problem.API (APIProblem(..))

data Scorer = forall a . (ScorerClass a, ExtractContest a) => Scorer a

contestToScorer :: Entity Contest -> Scorer
contestToScorer contestE = helper $ contestUrl $ entityVal contestE
    where
        -- helper "spring2015coursera" = Scorer $ ArtGallery contestE
        -- helper "fall2015coursera" = Scorer $ ATMSpec contestE
        -- helper "fall2015" = Scorer $ ATMSpec contestE
        -- helper "fall2016" = Scorer $ EHRSpec contestE
        helper _url = Scorer $ APIProblem contestE
        -- error $ "You must define Core.Modular.toModular for url: " ++ (Text.unpack url)

scorerLoop :: MVar Int -> Scorer -> RunnerOptions -> DatabaseM ()
scorerLoop exiting (Scorer scorer) runnerOptions = do
  lastRun <- liftIO $ getCurrentTime >>= newIORef
  
  checkExit exiting $ do
    -- Check for rescore.
    let contestId = extractContestId scorer
    rescores <- runDB $ selectList [ScorePendingContest ==. contestId] []
    now <- liftIO getCurrentTime
    flip mapM_ rescores $ \(Entity pendingId pending) -> do
        case scorePendingRound pending of
            ContestRoundBuild ->
                scoreContestBuild scorer runnerOptions
            ContestRoundBreak ->
                maybeScoreContestBreakFix scorer now runnerOptions lastRun
            ContestRoundFix ->
                maybeScoreContestBreakFix scorer now runnerOptions lastRun
        -- Delete the pending request. 
        runDB $ delete pendingId

    -- Rerun break/fix when lastrun was more than a minute ago.
    maybeScoreContestBreakFix scorer now runnerOptions lastRun
  
  where
    maybeScoreContestBreakFix scorer now runnerOptions lastRun = do
        lastRun' <- liftIO $ readIORef lastRun
        when (now `diffUTCTime` lastRun' > 60) $ do
            scoreContestBreakFix scorer now runnerOptions
            liftIO $ getCurrentTime >>= writeIORef lastRun


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

