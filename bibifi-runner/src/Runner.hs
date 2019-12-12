{-# LANGUAGE OverloadedStrings #-}
module Runner (runner, RunnerOptions(..)) where

import Control.Exception.Enclosed
-- import Control.Monad
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Reader
import Problem
import Data.Set (Set)
import Score
-- import System.Timeout

import Common
import Queue

getJob :: ProblemRunner -> (ProblemRunner -> Job -> DatabaseM (Maybe (Bool, Maybe ContestRound))) -> MVar (Set TeamContestId) -> MVar (Queue Job) -> MVar Int -> DatabaseM ()
getJob runner'@(ProblemRunner runner) f blockedTeams queue exiting = do
    -- Check for job.
    jobM <- popQueue queue
    case jobM of
        Just job -> do
            -- Prepare for timeout.
            -- dbConf <- ask
            -- 
            -- Run computation with timeout.
            -- let timer = 60 * 60 * 1000000
            -- let timer = 30
            -- resultM <- lift $ timeout timer $ runDatabaseM dbConf $ 
            --     runComputation (False, Nothing) $ f contest job
            -- Run computation. 
            resultM <- catchAny (f runner' job) $ \e -> do
                putLog $ show e
                return $ Just (False,Nothing)

            case resultM of
                Just ( success, rescore) -> 
                    -- Check if an error occured.
                    if success then do
                        -- Rescore if necessary.
                        let contestId = extractContestId runner
                        case rescore of
                            Just ContestRoundBuild -> 
                                rescoreBuildRound contestId
                            Just ContestRoundBreak ->
                                rescoreBreakRound contestId
                            Just ContestRoundFix ->
                                rescoreFixRound contestId
                            Nothing ->
                                return ()
                    else
                        -- Mark it as pending so that we can try to run it again. 
                        revertJob job
                Nothing -> do
                    -- Timeout occurred so record it.
                    putLog "Timeout occurred." -- Instances might be left running."
                    timeoutJob job

            -- Unlock team.
            let tcId = jobTeamContestId job
            unlockTeam tcId blockedTeams

            -- Sleep and loop.
            loop
        Nothing -> do
            -- If no jobs, check for exit. 
            exitM <- liftIO $ tryTakeMVar exiting
            case exitM of
                Nothing -> 
                    -- If not exiting, loop.
                    loop
                Just c -> do
                    -- If exiting, signal done. 
                    liftIO $ putMVar exiting $ c - 1
    where
        -- Sleep and then loop
        loop = do
            liftIO $ threadDelay 1000000
            getJob runner' f blockedTeams queue exiting

        jobTeamContestId (OracleJob (Entity _ os)) = oracleSubmissionTeam os
        jobTeamContestId (BuildJob (Entity _ bs)) = buildSubmissionTeam bs
        jobTeamContestId (BreakJob (Entity _ bs)) = breakSubmissionTeam bs
        jobTeamContestId (FixJob (Entity _ fs)) = fixSubmissionTeam fs

runner :: ProblemRunner -> RunnerOptions -> MVar (Set TeamContestId) -> MVar (Queue Job) -> MVar Int -> DatabaseM ()
runner r options = getJob r $ runJob options

revertJob :: Job -> DatabaseM ()
revertJob (OracleJob (Entity osId _os)) = runDB $ 
    update osId [OracleSubmissionStatus =. OraclePending]
revertJob (BuildJob (Entity bsId _bs)) = runDB $ 
    update bsId [BuildSubmissionStatus =. BuildPending]
revertJob (BreakJob (Entity bsId _bs)) = runDB $ 
    update bsId [] -- FIXME [BreakSubmissionStatus =. BreakPending]
revertJob (FixJob (Entity fsId _fs)) = runDB $
    update fsId [FixSubmissionStatus =. FixPending]

timeoutJob :: Job -> DatabaseM ()
timeoutJob (OracleJob (Entity osId _os)) = runDB $
    update osId [OracleSubmissionStatus =. OracleError, OracleSubmissionOutput =. Just "Timeout"]
timeoutJob (BuildJob (Entity bsId _bs)) = runDB $ 
    update bsId [BuildSubmissionStatus =. BuildTimeout]
timeoutJob (BreakJob (Entity bsId _bs)) = runDB $ 
    update bsId [] -- FIXME [BreakSubmissionStatus =. BreakTimeout]
timeoutJob (FixJob (Entity fsId _fs)) = runDB $ update fsId [FixSubmissionStatus =. FixTimeout]

runJob :: RunnerOptions -> ProblemRunner -> Job -> DatabaseM (Maybe (Bool, Maybe ContestRound))
runJob opts (ProblemRunner r) (OracleJob os) = do
    successM <- runOracleSubmission r opts os
    case successM of 
        Nothing ->
            return Nothing
        Just success ->
            return $ Just (success, Nothing)

runJob opts (ProblemRunner r) (BuildJob bs) = do
    -- TODO: download and save?
    resM <- runBuildSubmission r opts bs
    case resM of
        Nothing ->
            return Nothing
        Just (success, rescore') ->
            let rescore = if rescore' then Just ContestRoundBuild else Nothing in
            return $ Just (success, rescore)

runJob opts (ProblemRunner r) (BreakJob bs) = do
   -- TODO: download and save?
            bfs <- getBfs
            resM <- runBreakSubmission r opts bs bfs
            case resM of 
                Nothing ->
                    return Nothing
                Just (success, rescore') ->
                    let rescore = if rescore' then Just ContestRoundBreak else Nothing in
                    return $ Just (success, rescore)
  where
    getBfs = runDB $ do
        res <- selectFirst [BreakFixSubmissionBreak ==. entityKey bs]
                           [Asc BreakFixSubmissionId]
        case res of
            Just it -> return it
            Nothing -> error $ "getBfs: BreakFixSubmission does not exist" ++ show (entityKey bs)

runJob opts (ProblemRunner r) (FixJob fs) = do
    -- TODO: download and save?
    resM <- runFixSubmission r opts fs
    case resM of
        Nothing ->
            return Nothing
        Just (success, rescore') -> 
            let rescore = if rescore' then Just ContestRoundFix else Nothing in
            return $ Just (success, rescore)


-- getBuildTest blockedTeams = 
--     -- Get next build-it test for non-blocked teams.
--     testM <- runDB $ select $ from $ \buildTest -> do
--         where_ $ 
--         orderBy 
--         limit 1
--         return buildTest
--     case testM of
--         [] ->
--             return Nothing
--         [test] ->
--             return $ Just test
--         _ -> 
--             error "Unreachable due to database query"
    

-- runTest 
    -- Block team
    -- Set as running
    -- Run test
    -- store result
    -- Unblock team
