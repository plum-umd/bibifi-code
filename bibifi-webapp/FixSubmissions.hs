module FixSubmissions where

-- import Database.Esqueleto.Internal.Sql (SqlSelect)
-- 
-- import Import

-- getLatestFixSubmissions :: ( MonadResource m, SqlSelect a r, E.MonadSqlPersist m) => 
--     ContestId -> 
--     (E.SqlExpr (Entity TeamContest) -> E.SqlExpr (Entity FixSubmission) -> E.SqlQuery a) -> 
--     m [r]

-- getLatestFixSubmissions cId f = E.select $ E.from $ \(E.InnerJoin tc fs) -> do
--     E.on ( tc E.^. TeamContestId E.==. fs E.^. FixSubmissionTeam)
--     E.where_ ( tc E.^. TeamContestContest E.==. E.val cId)
--     f tc fs
