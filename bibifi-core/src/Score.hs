module Score (rescoreBuildRound, rescoreBreakRound, rescoreFixRound) where

import Database.Persist

import Core.Database
import Model
import PostDependencyType

rescoreHelper :: (
      GeneralPersistSql site m
    ) => ContestRound -> ContestId -> m ()
rescoreHelper round contestId = do
    _ <- runDB' $ insertUnique $ ScorePending contestId round
    return ()

rescoreBuildRound :: (
      GeneralPersistSql site m
    ) => ContestId -> m ()
rescoreBuildRound = rescoreHelper ContestRoundBuild

rescoreBreakRound :: (
      GeneralPersistSql site m
    ) => ContestId -> m ()
rescoreBreakRound = rescoreHelper ContestRoundBreak

rescoreFixRound :: (
      GeneralPersistSql site m
    ) => ContestId -> m ()
rescoreFixRound = rescoreHelper ContestRoundFix
