module Scorer.Class where

import Core.DatabaseM
import Problem.Class

class ScorerClass scorer where
    scoreContestBuild :: scorer -> RunnerOptions -> DatabaseM ()
    scoreContestBreak :: scorer -> RunnerOptions -> DatabaseM ()
    scoreContestFix :: scorer -> RunnerOptions -> DatabaseM ()
    
