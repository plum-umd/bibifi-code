module Scorer where

import Control.Monad.Trans.Control
import Common
import Problem.Class (ExtractContest(..), extractContestId, RunnerOptions(..))
import Scorer.Class

-- Import additional contest specification instances here.
import Core.Modular.ATM (ATMSpec(..))
import Core.Modular.ArtGallery (ArtGallery(..))
import Core.Modular.EHR (EHRSpec(..))

data Scorer = forall a . (ScorerClass a, ExtractContest a) => Scorer a

contestToScorer :: Entity Contest -> Scorer
contestToScorer contestE = helper $ contestUrl $ entityVal contestE
    where
        helper "spring2015coursera" = Scorer $ ArtGallery contestE
        helper "fall2015coursera" = Scorer $ ATMSpec contestE
        helper "fall2015" = Scorer $ ATMSpec contestE
        helper "fall2016" = Scorer $ EHRSpec contestE
        helper _url = undefined -- error $ "You must define Core.Modular.toModular for url: " ++ (Text.unpack url)

scorerLoop :: MVar Int -> Scorer -> RunnerOptions -> DatabaseM ()
scorerLoop exiting (Scorer scorer) runnerOptions = checkExit exiting $ do
    -- Check for rescore.
    let contestId = extractContestId scorer
    rescores <- runDB $ selectList [ScorePendingContest ==. contestId] []
    flip mapM_ rescores $ \(Entity pendingId pending) -> do
        case scorePendingRound pending of
            ContestRoundBuild ->
                scoreContestBuild scorer runnerOptions
            ContestRoundBreak ->
                scoreContestBreak scorer runnerOptions
            ContestRoundFix ->
                scoreContestFix scorer runnerOptions
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

