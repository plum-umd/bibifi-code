module Scorer.Class where

import Data.Time

import Core.DatabaseM
import Problem.Class

class ScorerClass scorer where
    scoreContestBuild :: scorer -> RunnerOptions -> DatabaseM ()
    scoreContestBreakFix :: scorer -> UTCTime -> RunnerOptions -> DatabaseM ()
    -- scoreContestFix :: scorer -> RunnerOptions -> DatabaseM ()
    
