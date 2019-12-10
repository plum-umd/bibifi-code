module Scheduler (scheduler) where

import Data.Set (Set)
import qualified Data.Set as Set
import Database.Esqueleto hiding ((=.), (==.))
import qualified Database.Esqueleto as E

import Common
import Queue

scheduler :: MVar (Queue Job) -> MVar Int -> MVar (Set TeamContestId) -> Entity Contest -> DatabaseM ()
scheduler queue exiting lockedTeams (Entity contestId _) = loopUntilExit $ do
    -- Get available teams.
    availableTeams <- getAvailableTeams contestId lockedTeams
    
    -- Get next oracle submission for available teams. 
    oracleM <- getOracleSubmission availableTeams
    case oracleM of
        Just oracle -> do
            -- Lock team.
            lockTeam (oracleSubmissionTeam $ entityVal oracle) lockedTeams

            -- Mark test as running.
            runDB $ updateWhere [OracleSubmissionId ==. entityKey oracle] [OracleSubmissionStatus =. OracleRunning]

            -- Add test to queue. 
            pushQueue (OracleJob oracle) queue
        Nothing -> do
            -- Get next build-it submission for available teams.
            buildM <- getBuildSubmission availableTeams
            case buildM of 
                Just build -> do
                    -- Lock team.
                    lockTeam (buildSubmissionTeam $ entityVal build) lockedTeams

                    -- Mark test as building.
                    runDB $ updateWhere [BuildSubmissionId ==. entityKey build] [BuildSubmissionStatus =. BuildBuilding]

                    -- Add test to queue. 
                    pushQueue (BuildJob build) queue
                Nothing -> do
                    -- Get next break-it submission for available teams. 
                    breakM <- getBreakSubmission availableTeams
                    case breakM of
                        Just break -> do
                            lockTeam (breakSubmissionTeam $ entityVal break) lockedTeams

                            runDB $ updateWhere [BreakSubmissionId ==. entityKey break] [{-BreakSubmissionStatus =. BreakTesting-}] -- FIXME
                            
                            pushQueue (BreakJob break) queue
                        Nothing -> do
                            -- Get next fix-it submission for available teams. (Maybe??)
                            fixM <- getFixSubmission availableTeams
                            case fixM of
                                Just fix -> do
                                    lockTeam (fixSubmissionTeam $ entityVal fix) lockedTeams

                                    runDB $ updateWhere [FixSubmissionId ==. entityKey fix] [FixSubmissionStatus =. FixBuilding]

                                    pushQueue (FixJob fix) queue
                                Nothing ->
                                    -- No jobs to do.
                                    return ()

    where
        loopUntilExit :: DatabaseM () -> DatabaseM ()
        loopUntilExit f = do
            -- Check for exiting.
            exitCountM <- liftIO $ tryReadMVar exiting
            case exitCountM of 
                Nothing -> do
                    -- Run computation.
                    f

                    -- Sleep.
                    liftIO $ threadDelay 1000000

                    -- Try again. 
                    loopUntilExit f
                Just 0 -> 
                    return ()
                Just _ -> do
                    -- Exiting so sleep.
                    liftIO $ threadDelay 1000000

                    -- Try again. 
                    loopUntilExit f

getAvailableTeams :: ContestId -> MVar (Set TeamContestId) -> DatabaseM [TeamContestId]
getAvailableTeams contestId lockedTeams = do
    lockedTeamsL <- fmap Set.toList $ liftIO $ readMVar lockedTeams
    tcIds <- runDB $ select $ from $ \tc -> do
        where_ $
            tc ^. TeamContestContest E.==. val contestId 
            &&. tc E.^. TeamContestId `notIn` (valList lockedTeamsL)
        return $ tc ^. TeamContestId
    return $ map unValue tcIds

maybeList :: [a] -> Maybe a
maybeList xs = case xs of
    [] -> 
        Nothing
    [x] -> 
        Just x
    _ -> 
        error "Should be unreachable due to database queries"

getOracleSubmission :: [TeamContestId] -> DatabaseM (Maybe (Entity OracleSubmission))
getOracleSubmission availableTeams = do
    -- Get next oracle submission for available teams.
    submissions <- runDB $ select $ from $ \os -> do
        where_ $ 
            (os ^. OracleSubmissionStatus E.==. val OraclePending) 
            &&. ((os ^. OracleSubmissionTeam) `in_` (valList availableTeams))
        orderBy [asc (os ^. OracleSubmissionTimestamp)]
        limit 1
        return os
    return $ maybeList submissions

getBuildSubmission :: [TeamContestId] -> DatabaseM (Maybe (Entity BuildSubmission))
getBuildSubmission availableTeams = do
    -- Get next pending build submission for available teams.
    submissions <- runDB $ select $ from $ \bs -> do
        where_ $
            bs ^. BuildSubmissionStatus E.==. val BuildPending
            &&. bs ^. BuildSubmissionTeam `in_` valList availableTeams
        orderBy [asc $ bs ^. BuildSubmissionTimestamp]
        limit 1
        return bs
    return $ maybeList submissions

getBreakSubmission :: [TeamContestId] -> DatabaseM (Maybe (Entity BreakSubmission))
getBreakSubmission availableTeams = do
    -- Get next pending.
    ss <- runDB $ select $ from $ \bs -> do
        where_ $ -- FIXME
            {-bs ^. BreakSubmissionStatus E.==. val BreakPending
            &&. -} bs ^. BreakSubmissionTeam `in_` valList availableTeams
        orderBy [asc $ bs ^. BreakSubmissionTimestamp]
        limit 1
        return bs
    return $ maybeList ss

getFixSubmission :: [TeamContestId] -> DatabaseM (Maybe (Entity FixSubmission))
getFixSubmission availableTeams = do
    ss <- runDB $ select $ from $ \fs -> do
        where_ $ 
            fs ^. FixSubmissionStatus E.==. val FixPending
            &&. fs ^. FixSubmissionTeam `in_` valList availableTeams
        orderBy [asc $ fs ^. FixSubmissionTimestamp]
        limit 1
        return fs
    return $ maybeList ss
