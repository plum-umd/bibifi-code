module BreakSubmissions where

import qualified Database.Esqueleto as E
import Database.Esqueleto.Internal.Sql (SqlSelect)

import Import
import PostDependencyType

-- getLatestBreakSubmissions :: ( MonadResource m, SqlSelect a r, E.MonadSqlPersist m) => 
--     ContestId -> 
--     (E.SqlExpr (Entity TeamContest) -> E.SqlExpr (Entity BreakSubmission) -> E.SqlQuery a) -> 
--     m [r]

-- getLatestBreakSubmissions cId f = E.select $ E.from $ \(E.InnerJoin tc bs) -> do
--     E.on ( tc E.^. TeamContestId E.==. bs E.^. BreakSubmissionTargetTeam)
--     E.where_ ( tc E.^. TeamContestContest E.==. E.val cId)
--     f tc bs

-- getBothTeams :: ( MonadResource m, SqlSelect a r, E.MonadSqlPersist m) =>
--     BreakSubmissionId ->
--     (E.SqlExpr (Entity Team)
--         -> E.SqlExpr (Entity TeamContest)
--         -> E.SqlExpr (Entity BreakSubmission)
--         -> E.SqlExpr (Entity TeamContest)
--         -> E.SqlExpr (Entity Team)
--         -> E.SqlQuery a) ->
--     m [r]
-- getBothTeams bsId f = E.select $ E.from $ \( E.InnerJoin t (E.InnerJoin tc (E.InnerJoin bs (E.InnerJoin tct tt)))) -> do
--     E.on ( tct E.^. TeamContestTeam E.==. tt E.^. TeamId)
--     E.on ( bs E.^. BreakSubmissionTargetTeam E.==. tct E.^. TeamContestId)
--     E.on ( bs E.^. BreakSubmissionTeam E.==. tc E.^. TeamContestId)
--     E.on ( tc E.^. TeamContestTeam E.==. t E.^. TeamId)
--     E.where_ ( bs E.^. BreakSubmissionId E.==. E.val bsId)
--     E.limit 1
--     f t tc bs tct tt
    
getBothTeams :: MonadIO m => BreakSubmissionId -> (Entity Team -> Entity TeamContest -> Entity BreakSubmission -> Maybe (Entity TeamContest) -> Maybe (Entity Team) -> a) -> (ReaderT SqlBackend m) (Maybe a)
getBothTeams bsId f = do
    res <- E.select $ E.from $ \( E.InnerJoin t (E.InnerJoin tc (E.LeftOuterJoin bs (E.InnerJoin tct tt)))) -> do
        E.on ( tct E.?. TeamContestTeam E.==. tt E.?. TeamId)
        E.on ( bs E.^. BreakSubmissionTargetTeam E.==. tct E.?. TeamContestId)
        E.on ( bs E.^. BreakSubmissionTeam E.==. tc E.^. TeamContestId)
        E.on ( tc E.^. TeamContestTeam E.==. t E.^. TeamId)
        E.where_ ( bs E.^. BreakSubmissionId E.==. E.val bsId)
        E.limit 1
        return (t, tc, bs, tct, tt)
    case res of
        [(t, tc, bs, tct, tt)] -> 
            return $ Just $ f t tc bs tct tt
        _ -> 
            return Nothing
    
checkBreakSubmissionTeam tcId bsId = do
    bs <- runDB $ get404 bsId
    when (breakSubmissionTeam bs /= tcId && breakSubmissionTargetTeam bs /= Just tcId) $
        notFound

    return bs

withdrawBreakSubmission bsId =
    update bsId [ BreakSubmissionValid =. Just False
                , BreakSubmissionMessage =. Just "Break resubmitted" 
                , BreakSubmissionStatus =. BreakRejected
                , BreakSubmissionWithdrawn =. True
                ]
                -- JP: Update status too?
