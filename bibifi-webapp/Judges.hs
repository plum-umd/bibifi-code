{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}

module Judges where

import qualified Data.List as List
import qualified Database.Esqueleto as E
import Database.Persist.Sql (SqlBackend)

import Import

layout :: Text -> (UserId -> Entity Contest -> Key Judge -> LWidget) -> LHandler Html
layout url f = do
    raiseJudgeLabel
    -- Get contest.
    contestM <- runDB $ getBy $ UniqueContest url
    let title = generatePageTitle contestM "Judge Queue"
    customLayout contestM $ contestTemplate contestM "Judge Queue" $ \contest' -> do
        uId <- handlerToWidget $ requireAuthId
        setTitle $ toHtml title
        let (Entity cId _contest) = contest'
        -- Check if current user is a judge for this contest.
        judgeM <- handlerToWidget $ runDB $ getBy $ UniqueContestJudge uId cId
        case judgeM of
            Nothing ->
                notFound
            Just (Entity judgeId _) ->
                f uId contest' judgeId

-- updateJudgeCount :: ( PersistQuery m, PersistMonadBackend m ~ SqlBackend) => 
--     (Int -> Int) -> 
--     JudgeId -> 
--     m ()
updateJudgeCount f jId = do
    jM <- get jId
    case jM of
        Nothing ->
            return ()
        Just j ->
            let newCount = f $ judgeAssignedCount j in
            update jId [JudgeAssignedCount =. newCount]

-- incrementCount :: ( PersistQuery m, PersistMonadBackend m ~ SqlBackend) => 
--     JudgeId -> m ()
incrementCount = updateJudgeCount $ (+) 1

-- decrementCount :: ( PersistQuery m, PersistMonadBackend m ~ SqlBackend) => 
--     JudgeId -> m ()
decrementCount = updateJudgeCount $ (flip (-)) 1
        
-- getOrdered :: ( PersistMonadBackend m ~ E.SqlBackend, MonadResource m, E.MonadSqlPersist m, ContestSubmission k, PersistStore m) =>
--     ContestId -> k -> m [(Text, JudgeId)]
getOrdered cId submission = do
    conflicted <- getInvolvedTeams submission
    let guard u j jc = 
          let base = (j E.^. JudgeContest E.==. E.val cId) in
          case conflicted of 
            [] -> 
                base
            h:t ->
                let conds = List.foldl' (\acc tc ->
                        (jc E.?. JudgeConflictTeam E.!=. E.just (E.val tc)) E.&&. acc
                      ) (jc E.?. JudgeConflictTeam E.!=. E.just (E.val h)) t
                in
                base E.&&. (E.isNothing (jc E.?. JudgeConflictTeam) E.||. conds)
    -- let guard u j jc = List.foldl' (\acc tc -> 
    --         (jc E.?. JudgeConflictTeam E.!=. E.just (E.val tc)) E.&&. acc
    --       ) (j E.^. JudgeContest E.==. E.val cId) conflicted
    judges <- E.select $ E.from $ \(E.InnerJoin u (E.LeftOuterJoin j jc)) -> do
        E.on ( E.just (j E.^. JudgeId) E.==. jc E.?. JudgeConflictJudge)
        E.on (u E.^. UserId E.==. j E.^. JudgeJudge)
        E.where_ (guard u j jc)
        E.orderBy [E.asc (j E.^. JudgeAssignedCount)]
        return (u E.^. UserIdent, j E.^. JudgeId)
    -- judges <- foldM (\acc (E.Value u, E.Value jId)el
    --         let 
    --         s
    --     ) [] judges'
    return $ map (\(Value u, Value jId) -> (u,jId)) judges

instance ContestSubmission BuildSubmissionId where
    getInvolvedTeams sId = do 
        bsM <- E.get $ sId
        return $ case bsM of
            Nothing ->
                []
            Just bs -> do
                [buildSubmissionTeam bs]

instance ContestSubmission BreakSubmissionId where
    getInvolvedTeams sId = do
        bsM <- E.get $ sId
        return $ maybe [] (\bs -> [breakSubmissionTeam bs, breakSubmissionTargetTeam bs]) bsM

instance ContestSubmission FixSubmissionId where
    getInvolvedTeams sId = do
        bsM <- E.get $ sId
        return $ maybe [] (\bs -> [fixSubmissionTeam bs]) bsM
        -- TODO: add teams that made breaks too?
        
