{-# LANGUAGE ScopedTypeVariables #-}

module BuildSubmissions where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.List as List
import Database.Persist
import qualified Database.Esqueleto as E
import Database.Esqueleto.Internal.Sql (SqlSelect)

import Model

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
