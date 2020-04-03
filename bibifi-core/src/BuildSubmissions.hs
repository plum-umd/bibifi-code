{-# LANGUAGE ScopedTypeVariables #-}

module BuildSubmissions where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.List as List
import Data.Time.Clock (UTCTime)
import Database.Persist
import qualified Database.Esqueleto as E
import Database.Esqueleto.Internal.Sql (SqlSelect)

import Model
import PostDependencyType
    ( FixSubmissionResult(..), BreakSubmissionResult(..), BuildSubmissionStatus(..))

getLatestBuildSubmissions :: (MonadIO m, SqlSelect a b) => ContestId -> (E.SqlExpr (Entity TeamContest) -> E.SqlExpr (Entity BuildSubmission) -> E.SqlQuery a) -> E.SqlPersistT m [b]
getLatestBuildSubmissions cId f = do
    E.select $ E.from $ \(E.InnerJoin tc (E.LeftOuterJoin bs bs')) -> do
        -- E.on ( E.just ( bs E.^. BuildSubmissionTimestamp) E.<. bs' E.?. BuildSubmissionTimestamp E.&&. E.just ( bs E.^. BuildSubmissionTeam) E.==. bs' E.?. BuildSubmissionTeam)
        -- Now assumes that BuildSubmissionId is monotonically increasing. 
        E.on ( E.just ( bs E.^. BuildSubmissionId) E.<. bs' E.?. BuildSubmissionId E.&&. E.just ( bs E.^. BuildSubmissionTeam) E.==. bs' E.?. BuildSubmissionTeam)
        E.on ( tc E.^. TeamContestId E.==. bs E.^. BuildSubmissionTeam)
        E.where_ ( tc E.^. TeamContestContest E.==. E.val cId E.&&. E.isNothing (bs' E.?. BuildSubmissionTeam) E.&&. tc E.^. TeamContestProfessional E.==. E.val False)
        f tc bs

buildSubmissionPassesRequiredTests :: (MonadIO m) => ContestId -> BuildSubmissionId -> ReaderT E.SqlBackend m Bool
-- buildSubmissionPassesRequiredTests :: (PersistQueryRead backend,
--      MonadIO m, BackendCompatible E.SqlBackend backend,
--      PersistUniqueRead backend,
--      BaseBackend backend ~ E.SqlBackend) =>
--     Key Contest -> Key BuildSubmission -> ReaderT backend m Bool
-- buildSubmissionPassesRequiredTests :: (MonadIO m, BaseBackend backend ~ E.SqlBackend, BackendCompatible E.SqlBackend backend, PersistUniqueRead backend, PersistQueryRead backend) => ContestId -> BuildSubmissionId -> ReaderT backend m Bool
buildSubmissionPassesRequiredTests cId bsId = do
    -- Total number of required tests
    numCoreTests <- count [ContestCoreTestContest ==. cId]
    numPerformanceTests <- count [ContestPerformanceTestContest ==. cId, ContestPerformanceTestOptional ==. False]

    -- Check that every required test was passed.
    numPassedCoreTests <- count [BuildCoreResultSubmission ==. bsId, BuildCoreResultPass ==. True]
    -- [numPassedPerformanceTests] <- E.select $ E.from $ \(E.InnerJoin result test) -> do
    --     E.on (result E.^. BuildPerformanceResultTest E.==. test E.^. ContestPerformanceTestId)
    --     E.where_ ( result E.^. BuildPerformanceResultSubmission E.==. E.val bsId 
    --         E.&&. test E.^. ContestPerformanceTestOptional E.==. E.val False
    --         E.&&. result E.^. BuildPerformanceResultTime E.!=. E.nothing
    --         )
    --     return E.countRows 
    -- TODO: Migrate this to countRows XXX
    tmps <- E.select $ E.from $ \(E.InnerJoin result test) -> do
        E.on (result E.^. BuildPerformanceResultTest E.==. test E.^. ContestPerformanceTestId)
        E.where_ ( result E.^. BuildPerformanceResultSubmission E.==. E.val bsId 
            E.&&. test E.^. ContestPerformanceTestOptional E.==. E.val False
            E.&&. E.not_ (E.isNothing (result E.^. BuildPerformanceResultTime))
            )
        return $ result E.^. BuildPerformanceResultId
    let numPassedPerformanceTests = List.length tmps
    return $ numPassedCoreTests == numCoreTests && numPassedPerformanceTests == numPerformanceTests

getLatestBuildOrFix :: MonadIO m => Contest -> TeamContestId -> UTCTime -> E.SqlPersistT m (Either String (Either BuildSubmissionId FixSubmissionId))
-- Retrieve the latest successful fix or build for team
getLatestBuildOrFix contest teamId time = do
    latestBuild <- selectFirst [ BuildSubmissionTeam ==. teamId, BuildSubmissionTimestamp <=. contestBuildEnd contest]
                               [ Desc BuildSubmissionTimestamp ]
    latestFix   <- selectFirst [ FixSubmissionTeam ==. teamId
                               , FixSubmissionResult ==. Just FixFixed
                               , FixSubmissionTimestamp <=. time ]
                               [ Desc FixSubmissionTimestamp ]
    return $ case (latestBuild, latestFix) of
        (_, Just (Entity id _))  -> Right $ Right id -- (fixSubmissionCommitHash f, Just id)
        (Just (Entity id _), _)   -> Right $ Left id -- (buildSubmissionCommitHash b, Nothing)
        _                        -> Left "No valid target"

-- isValidBreakTeam :: ContestId -> TeamContestId -> DatabaseM Bool
-- Builder team qualified for breakit?
isQualifiedBuilderTeam :: MonadIO m => Entity Contest -> Key TeamContest -> ReaderT E.SqlBackend m Bool
isQualifiedBuilderTeam (Entity contestId contest) targetTeamId = do
    bsId <- selectFirst [BuildSubmissionTeam ==. targetTeamId, BuildSubmissionTimestamp <=. contestBuildEnd contest] [Desc BuildSubmissionId]
    case bsId of
        Just (Entity bsId bs) | qualifiedBuildSubmission bs -> do
            buildSubmissionPassesRequiredTests contestId bsId
        _ ->
            return False
  
  where
    qualifiedBuildSubmission bs = 
        let st = buildSubmissionStatus bs in
        st == BuildBuilt

-- JP: Should probably put this in a new module...
-- Assumes the break is valid.
isActiveBreak :: (MonadIO m) => Key BreakSubmission -> E.SqlPersistT m Bool
isActiveBreak bsId = do
    n <- E.select $ E.from $ \(E.InnerJoin bfs fs) -> do
        E.on (bfs E.^. BreakFixSubmissionFix E.==. E.just (fs E.^. FixSubmissionId))
        E.where_ (
                bfs E.^. BreakFixSubmissionBreak E.==. E.val bsId
          E.&&. fs E.^. FixSubmissionResult E.==. E.just (E.val FixFixed)
          E.&&. bfs E.^. BreakFixSubmissionResult E.==. E.val BreakFailed
          )
        return $ fs E.^. FixSubmissionId

    -- Fixed if there exists an accepted fix and the break failed.
    return $ length n == 0

