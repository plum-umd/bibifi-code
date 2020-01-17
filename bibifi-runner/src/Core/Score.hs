{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Core.Score (defaultScoreBuildRound, defaultScoreBreakFixRound) where

import BuildSubmissions (buildSubmissionPassesRequiredTests, isQualifiedBuilderTeam)
import Core.DatabaseM
import Control.Monad
import Control.Monad.Trans.Class
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M
-- import qualified Data.Function as F
import qualified Data.List as L
import Data.Maybe
import Data.Time.Clock
import qualified Database.Esqueleto as E
import Database.Persist
import Model
import PostDependencyType

import Problem.Shared

type ScoreMap = Map TeamContestId (Double, Double, Double, Double)

-- startScore = -1000.0
-- baselineScore = 100.0
baselineConstant :: Double
baselineConstant = 5.0

instance Hashable TeamContestId where
    hashWithSalt s k' = case keyToValues k' of
        [PersistInt64 i] ->
            hashWithSalt s i
        _ -> 
            error "this shouldn't happen with a pgsql db"
        
updateBuildBuildScore :: TeamContestId -> Maybe Double -> DatabaseM ()
updateBuildBuildScore teamId score = do
    oldScore' <- runDB $ selectFirst [TeamBuildScoreTeam ==. teamId] [Desc TeamBuildScoreTimestamp]
    now <- lift getCurrentTime
    case oldScore' of
        Just (Entity _ oldScore) ->
            -- Don't insert duplicates.
            unless ( teamBuildScoreBuildScore oldScore == score) $
                let newScore = oldScore {teamBuildScoreBuildScore = score, teamBuildScoreTimestamp = now} in
                runDB $ insert_ newScore
        Nothing ->
            let newScore = TeamBuildScore teamId score Nothing Nothing now in
            runDB $ insert_ newScore

updateBuildBreakScore :: UTCTime -> (TeamContestId, Double) -> DatabaseM ()
updateBuildBreakScore now ( teamId, score) = do
    oldScore' <- runDB $ selectFirst [TeamBuildScoreTeam ==. teamId] [Desc TeamBuildScoreTimestamp]
    case oldScore' of
        Just (Entity _ oldScore) ->
            -- Don't insert duplicates.
            unless ( teamBuildScoreBreakScore oldScore == Just score) $
                let newScore = oldScore {teamBuildScoreBreakScore = Just score, teamBuildScoreTimestamp = now} in
                runDB $ insert_ newScore
        Nothing ->
            let newScore = TeamBuildScore teamId Nothing (Just score) Nothing now in
            runDB $ insert_ newScore

updateBreakBuildScore :: UTCTime -> (TeamContestId, Double) -> DatabaseM ()
updateBreakBuildScore now ( teamId, score) = do
    oldScore' <- runDB $ selectFirst [TeamBreakScoreTeam ==. teamId] [Desc TeamBreakScoreTimestamp]
    case oldScore' of
        Just (Entity _ oldScore) ->
            -- Don't insert duplicates.
            unless ( teamBreakScoreBuildScore oldScore == Just score) $
                let newScore = oldScore {teamBreakScoreBuildScore = Just score, teamBreakScoreTimestamp = now} in
                runDB $ insert_ newScore
        Nothing ->
            let newScore = TeamBreakScore teamId (Just score) Nothing Nothing now in
            runDB $ insert_ newScore

updateBreakBreakScore :: UTCTime -> (TeamContestId, Double) -> DatabaseM ()
updateBreakBreakScore now ( teamId, score) = do
    oldScore' <- runDB $ selectFirst [TeamBreakScoreTeam ==. teamId] [Desc TeamBreakScoreTimestamp]
    case oldScore' of
        Just (Entity _ oldScore) ->
            -- Don't insert duplicates.
            unless ( teamBreakScoreBreakScore oldScore == Just score) $ 
                let newScore = oldScore {teamBreakScoreBreakScore = Just score, teamBreakScoreTimestamp = now} in
                runDB $ insert_ newScore
        Nothing ->
            let newScore = TeamBreakScore teamId Nothing (Just score) Nothing now in
            runDB $ insert_ newScore

updateBuildFixScore :: UTCTime -> (TeamContestId, Double) -> DatabaseM ()
updateBuildFixScore now ( teamId, score) = do
    oldScore' <- runDB $ selectFirst [TeamBuildScoreTeam ==. teamId] [Desc TeamBuildScoreTimestamp]
    case oldScore' of
        Just (Entity _ oldScore) ->
            -- Don't insert duplicates.
            unless ( teamBuildScoreFixScore oldScore == Just score) $ 
                let newScore = oldScore {teamBuildScoreFixScore = Just score, teamBuildScoreTimestamp = now} in
                runDB $ insert_ newScore
        Nothing ->
            let newScore = TeamBuildScore teamId Nothing Nothing (Just score) now in
            runDB $ insert_ newScore

updateBreakFixScore :: UTCTime -> (TeamContestId, Double) -> DatabaseM ()
updateBreakFixScore now ( teamId, score) = do
    oldScore' <- runDB $ selectFirst [TeamBreakScoreTeam ==. teamId] [Desc TeamBreakScoreTimestamp]
    case oldScore' of
        Just (Entity _ oldScore) ->
            -- Don't insert duplicates.
            unless ( teamBreakScoreFixScore oldScore == Just score) $ 
                let newScore = oldScore {teamBreakScoreFixScore = Just score, teamBreakScoreTimestamp = now} in
                runDB $ insert_ newScore
        Nothing -> 
            let newScore = TeamBreakScore teamId Nothing Nothing (Just score) now in
            runDB $ insert_ newScore

addScore :: (Num n) => [(TeamContestId, n)] -> (TeamContestId, n) -> [(TeamContestId, n)]
addScore scores teamScore = 
    -- rev_append
    let helper_found scores acc = case scores of
          [] ->
              acc
          h:t ->
              helper_found t (h:acc)
    in 
    let (teamid, score) = teamScore in
    let helper scores acc =
          case scores of
            [] ->
                teamScore:acc
            h:t ->
                let (teamid',score') = h in
                if teamid == teamid' then
                    helper_found t (( teamid, score + score'):acc)
                else
                    helper t (h:acc)
    in
    helper scores []

defaultScoreBuildRound :: ContestId -> DatabaseM ()
defaultScoreBuildRound cId = do
    -- Produce list of teams
    teams <- runDB $ selectList [TeamContestContest ==. cId] []
    -- Produce list of submissions
    submissions <-
        let helper acc (Entity teamId _) = 
              -- don't think this'll work... 
              -- -- select * from BuildSubmission, BuildPerformanceResult where BuildPerformanceResult.submission = BuildSubmissionId and BuildPerformanceResult.test == ptId and BuildSubmission.team == teamId asc/des by BuildSubmission.timestamp LIMIT 1
              -- (liftDB g) $ runDB $ E.select $ E.from $ \( bs `E.LeftOuterJoin` bpr) -> do
              --     E.on (bpr E.^. BuildPerformanceResultSubmission E.==. bs E.^. BuildSubmissionId)
              --     E.where_ (( bpr E.^. BuildPerformanceResultTest E.==. (E.val ptId)) 
              --         E.&&. ( bs E.^. BuildSubmissionTeam E.==. (E.val teamId)))
              --     E.orderBy [E.desc (bs E.^. BuildSubmissionTimestamp)]
              --     E.limit 1
              --     return ( teamId, bs E.^. BuildSubmissionTimestamp)
              
              
              do
              submissionM <- runDB $ selectFirst [BuildSubmissionTeam ==. teamId, BuildSubmissionStatus !=. BuildBuilding, BuildSubmissionStatus !=. BuildPending] [Desc BuildSubmissionId] -- Desc BuildSubmissionTimestamp, 
              case submissionM of
                Nothing ->
                    return acc
                Just submissionE@(Entity sId _) -> do
                    -- Filter out submissions with failed judgements.
                    judgementM <- runDB $ getBy $ UniqueBuildJudgement sId
                    case judgementM of
                        Just (Entity _ j) | buildJudgementRuling j == Just False -> do
                            -- Set the user's score to nothing (disqualified). 
                            updateBuildBuildScore teamId Nothing
                            return acc
                        _ ->
                            return $ submissionE:acc


              -- res <- (liftDB g) $ runDB $ E.select $ E.from $ \bs -> do
              --     E.where_ ((( bs E.^. BuildSubmissionTeam) E.==. (E.val teamId))
              --         E.&&. ((bs E.^. BuildSubmissionStatus) E./=. (E.val BuildBuilding)))
              --     E.orderBy [E.desc (bs E.^. BuildSubmissionTimestamp)]
              --     E.limit 1
              --     return bs
              -- case res of 
              --     [s] ->
              --         return $ Just ( t, s)
              --     -- [(Entity sId s)] ->
              --     --     if buildSubmissionStatus s /= BuildBuilt then
              --     --         return Nothing
              --     --     else
              --     --         res' <- (liftDB g) $ runDB $ E.select $ E.from $ \bpr -> do
              --     --             E.where_ (bpr E.^. BuildPerformanceResultSubmission E.==. sId)
              --     _ ->
              --         return Nothing
        in
        foldM helper [] teams

    -- Get number of teams
    let m = 50 -- fromIntegral $ length teams

    -- Compute performance rankings
    perfTests <- runDB $ selectList [ContestPerformanceTestContest ==. cId] []
    let rankPerformances scores (Entity ptId _pt) = do
          -- ( team, time) list
          teamTimes <- 
            let helper acc (Entity sId s) = do
                  res <- runDB $ getBy $ UniqueBuildPerformanceSubmission sId ptId
                  case res of 
                      Nothing -> do
                          -- do
                          -- printError "warning: scorer: performance result not found for a submission"
                          return acc
                      Just (Entity _ bpr) ->
                          case buildPerformanceResultTime bpr of
                              Nothing ->
                                  return acc
                              Just t ->
                                  return $ ( buildSubmissionTeam s, t):acc
            in
            foldM helper [] submissions

          return $ updateGradesWithPerformanceTest m scores teamTimes

          -- Old performance test grading. 
          -- -- ( team, rank) list
          -- runDB $ deleteWhere [PerformanceTestRankingTest ==. ptId]
          -- teamRanks <-
          --       let rank (acc,res) l = 
          --             let r = acc + (length l) in
          --             do
          --             res' <- foldM (\ acc' ( tId, _) -> do
          --                       -- Insert into db
          --                       _ <- runDB $ insert $ PerformanceTestRanking ptId tId r
          --                       return $ ( tId, r):acc'
          --                   ) res l
          --             return ( r, res')
          --       in
          --       let l = L.groupBy ((==) `F.on` snd) $ L.sortBy (compare `F.on` snd) teamTimes in
          --       do
          --       l' <- foldM rank (0,[]) l
          --       return $ snd $ l'
          -- -- let p = length teamRanks
          -- let p = length teams

          -- -- ( team, point) list
          -- return $ L.foldl' (\ acc ( teamId, rank) -> addScore acc ( teamId, p - rank)) scores teamRanks

    -- ( team, point) list
    scores <- foldM rankPerformances [] perfTests

    -- foreach ContestPerformanceTest pt
    --  -- Get each team's latest
    --  foldover team in teams
    --      select * from ...BuildPerformanceResultTest = pt where submission ==. BuildSubmissionId && BuildSubmissionTeam ==. team
    --      return (team, time)
    --
    --      
    --
    --
    --
    --  produce (team, rank) list, and repsert into db
    --  or return (team, points) list???
    
    let scoreSubmission (Entity sId sub) = do
          let teamId = buildSubmissionTeam sub
          -- Check that every required test was passed.
          passesRequiredTests <- runDB $ buildSubmissionPassesRequiredTests cId sId
          if not passesRequiredTests then
            updateBuildBuildScore teamId Nothing
          else do
            numOptional <- runDB $ count [BuildOptionalResultSubmission ==. sId, BuildOptionalResultPass ==. True]
            let score = 
                  let optScore = case lookup teamId scores of
                        Nothing ->
                            0.0 :: Double
                        Just s ->
                            s
                  in
                  optScore + (baselineConstant * m) + ((fromIntegral numOptional) * m)/2.0
            updateBuildBuildScore teamId $ Just score
    mapM_ scoreSubmission submissions

    -- Add break points for breaking oracle.
    oracleBreaks <- runDB $ selectList [ BreakOracleSubmissionValid ==. True, BreakOracleSubmissionTeam <-. map entityKey teams] []
    let breakScores = L.foldl' (\scores (Entity _ ob) -> 
            addScore scores ( breakOracleSubmissionTeam ob, m/2.0)
          ) [] oracleBreaks

    now <- lift getCurrentTime
    mapM_ (updateBreakBuildScore now) breakScores

    where
        -- Update scores given performance results
        updateGradesWithPerformanceTest :: Double -> [(TeamContestId, Double)] -> [(TeamContestId, Double)] -> [(TeamContestId, Double)]
        updateGradesWithPerformanceTest m oldScores teamTimes = 
            -- Find slowest and fastest time.
            let (slowest, fastest) = L.foldl' (\(slowest, fastest) (_, t) -> 
                    (max slowest t, min fastest t)
                  ) ((-1/0), (1/0)) teamTimes
            in
            -- Fold over each team's time and update their score.
            L.foldl' (\oldScores ( teamId, time) -> 
                let score = 
                      -- All tied for first (avoid div by 0).
                      if slowest == fastest then
                        m
                      else
                        m * (slowest - time) / (slowest - fastest)
                in
                addScore oldScores ( teamId, score)
              ) oldScores teamTimes

    -- compute performance rankings (store as (testid, (team, rank) list) assoc list, and insert into db)
    -- for each team
    --  check if all the core tests are passed, otherwise return -1000
    --  return 100 + optional + performance
    --  set score in db

-- isBreakCrash :: BreakSubmission -> Bool
-- isBreakCrash submission =
--     let breakTypeM = breakSubmissionBreakType submission in
--     breakTypeM == Just BreakCrash

defaultScoreBreakFixRound :: Entity Contest -> UTCTime -> DatabaseM ()
defaultScoreBreakFixRound contestE@(Entity _ c) now = do

    -- Get all qualified builder teams.
    builderTeams <- runDB $ getQualifiedBuilders contestE

    -- Map TeamContestId (BuildLost, BreakGained, BreakLost, BuildGained)
    let scores' = mempty

    -- Fold over each team.
    scores <- foldM (scoreTeam now) scores' builderTeams

    -- Record scores.
    M.foldrWithKey (\tcId (buildLost, breakGained, breakLost, buildGained) m -> do
        m

        -- TODO: Should combine these functions.
        updateBuildBreakScore now (tcId, buildLost)
        updateBuildFixScore now (tcId, buildGained)
        updateBreakBreakScore now (tcId, breakGained)
        updateBreakFixScore now (tcId, breakLost)
      ) (return ()) scores
    
  where
    -- getQualifiedBuilders :: ContestId -> DatabaseM [TeamContestId]
    getQualifiedBuilders contestE = do
        teams <- fmap entityKey <$> selectList [TeamContestContest ==. entityKey contestE] []
        filterM (isQualifiedBuilderTeam contestE) teams

    scoreTeam now scores' targetTeamId = do
        -- Get all valid, accepted breaks against the team (in ascending ordered).
        validBreaks <- runDB $ getValidBreaks targetTeamId now

        -- Map (Maybe (FixId, Timestamp)) [Entity BreakSubmission]
        resM' <- foldM (\acc bsE@(Entity bsId _bs) -> do
            -- Get all break fix submission for each break inner joined with fix submissions, ordered by fix timestamp, fix id, before now.
            fs <- runDB $ E.select $ E.from $ \(bfs `E.InnerJoin` fs) -> do
                E.on (bfs E.^. BreakFixSubmissionFix E.==. E.just (fs E.^. FixSubmissionId))
                E.where_ (
                        bfs E.^. BreakFixSubmissionBreak E.==. E.val bsId
                  -- E.&&. bfs E.^. BreakFixSubmissionResult E.==. E.val BreakSucceeded
                  E.&&. bfs E.^. BreakFixSubmissionResult E.==. E.val BreakFailed
                  E.&&. fs E.^. FixSubmissionResult E.==. E.just (E.val FixFixed)
                  E.&&. fs E.^. FixSubmissionTimestamp E.<=. E.val now
                  )
                E.orderBy [E.asc (fs E.^. FixSubmissionTimestamp), E.asc (fs E.^. FixSubmissionId)]
                return fs

            -- Grab the first accepted fix.
            let fM = (\(Entity fsId fs) -> (fsId, fixSubmissionTimestamp fs)) <$> listToMaybe fs
            
            -- Group these breaks by the fix that fixed it (or not fixed)
            return $ M.insertWith (++) fM [bsE] acc
          ) mempty validBreaks

        -- Reverse breaks so they're in the right order.
        let resM = L.reverse <$> resM'


        -- Sort by timestamp + allocate/remove points.
        -- Cap end time to fix round end date.
        return $ M.foldrWithKey (scoreFix targetTeamId) scores' resM

    
    -- Unfixed breaks, so given them full points.
    scoreFix targetTeamId Nothing breaks scores' = 
        -- let breakScores = map (scoreBreakIt capTime) breaks in
        scoreBreakIt targetTeamId scores' capTime breaks
        -- addScores scores' breakScores
        
    -- Score all the breaks for a given fix.
    scoreFix targetTeamId (Just (_fixId, fixTime)) breaks scores = 
        let endTime = min capTime fixTime in
        let bScores' = scoreBreakIt targetTeamId mempty endTime breaks in
        let bScores = convertBreakItScores bScores' in
        let fScores = scoreFixIt targetTeamId scores endTime breaks in
        addScores bScores fScores

    -- BuildLost, BreakGained, BreakLost, BuildGained
    convertBreakItScores = fmap (\(buildLost, breakGained, breakLost, buildGained) -> (buildLost, breakGained, breakLost - breakGained, buildGained - buildLost))




        -- let startTime = snd $ L.head breaks in -- There should always be at least one break.

        -- L.foldl' (\(correctnessC, crashC, securityC) (Entity bsId bs) -> 
        --   
        --   ) (0,0,0) breaks
          
    capTime = min now (contestFixEnd c)

addScores :: ScoreMap -> ScoreMap -> ScoreMap
addScores = M.unionWith combineScores

combineScores :: (Double, Double, Double, Double) -> (Double, Double, Double, Double) -> (Double, Double, Double, Double)
combineScores (a1, a2, a3, a4) (b1, b2, b3, b4) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4)

insertScore scores tcId score = M.insertWith combineScores tcId score scores

scoreBreakIt _ scores' _ [] = scores'
scoreBreakIt targetTeamId scores'' endTime ((Entity _ bs):breaks) = 
    let m = breakTypeToM (breakSubmissionBreakType bs) in
    let diff = scoreOverTime m (breakSubmissionTimestamp bs) endTime in
    let scores' = insertScore scores'' targetTeamId (-diff, 0, 0, 0) in
    let scores = insertScore scores' submitTeamId (0, diff, 0, 0) in
    scoreBreakIt targetTeamId scores endTime breaks

  where
    submitTeamId = breakSubmissionTeam bs

scoreFixIt _ scores'' _ [] = scores'' -- Unreachable?
scoreFixIt targetTeamId scores'' endTime breaks@(firstBreakE:_) = 
    let firstStart = breakSubmissionTimestamp $ entityVal firstBreakE in
    let periods = breaksToPeriods endTime firstStart mempty mempty breaks in
    let scores' = scorePeriods targetTeamId scores'' firstStart endTime periods in
    scores'

scorePeriods :: TeamContestId -> ScoreMap -> UTCTime -> UTCTime -> [(UTCTime, UTCTime, Map TeamContestId (Int, Int, Int))] -> ScoreMap
scorePeriods _ scores _ _ [] = scores
scorePeriods targetTeamId scores startTime endTime ((periodStart, periodEnd, counts):periods) = 
    let splitC = fromInteger $ toInteger $ M.size counts in
    let scores' = M.foldlWithKey (\scores tcId count -> 
            let m = countToM count in
            let s = scorePeriod m startTime endTime periodStart periodEnd / splitC in
            -- let scores' = insertScore scores targetTeamId (0,0,-s,0) in
            -- insertScore scores' tcId (0,0,0,s)
            let scores' = insertScore scores targetTeamId (0,0,0,-s) in
            insertScore scores' tcId (0,0,s,0)
          ) scores counts 
    in
    scorePeriods targetTeamId scores' startTime endTime periods

  where
    countToM (_,_,exploitC) | exploitC >= 1 = breakTypeToM $ Just BreakSecurity
    countToM (_,crashC,_) | crashC >= 1 = breakTypeToM $ Just BreakCrash
    countToM (correctnessC,_,_) | correctnessC >= 1 = breakTypeToM $ Just BreakCorrectness
    countToM _ = error "countToM: unreachable"

    scorePeriod m startTime endTime periodStart periodEnd 
      | startTime > endTime = 0
      | periodStart > periodEnd = 0

      | endTime `diffUTCTime` startTime < fromInteger bufferTime = 
            -- (periodEnd - periodStart) / (endTime - startTime) * m
            m * ((timeToDouble (periodEnd `diffUTCTime` periodStart)) / (timeToDouble (endTime `diffUTCTime` startTime)))

      | otherwise = 
            scoreOverTime m startTime periodEnd - scoreOverTime m startTime periodStart


      --       -- Split into pre/post buffer.
      --       let (preStart, preEnd, postStart, postEnd) = 
      --               -- Before buffer time.
      --               if (periodStart <= bufferTime && periodEnd <= bufferTime) then
      --                   (periodStart, periodEnd, endTime, endTime)
      --               -- Overlaps with buffer time.
      --               else if (periodStart <= bufferTime && periodEnd > bufferTime) then
      --                   (periodStart, bufferTime, bufferTime, periodEnd)
      --               -- After buffer time.
      --               else
      --                   (startTime, startTime, periodStart, periodEnd)
      --       in
      --       let preScore = m * ((timeToDouble (preEnd `diffUTCTime` preStart)) / fromInteger bufferTime) in
      --       let postScore = scoreOverTime m startTime postEnd - scoreOverTime m startTime postStart in
      --       preScore + postScore

      -- where
      --   bufferTime = addUTCTime bufferTime startTime

    timeToDouble = fromRational . toRational


-- Assumes breaks are sorted by timestamp.
-- breaksToPeriods ::    -> [Entity BreakSubmission] -> [(UTCTime, UTCTime, Map AttackerTeamContestId (Int, Int, Int))]
breaksToPeriods endTime periodStart currentCounts acc [] = L.reverse $ (periodStart, endTime, currentCounts):acc
breaksToPeriods endTime periodStart currentCounts acc ((Entity _ bs):breaks) = 
    let currentCounts' = M.insertWith combineCounts attackerId (breakToCount bs) currentCounts in
    if periodStart == breakSubmissionTimestamp bs then
        -- Add to current period.
        breaksToPeriods endTime periodStart currentCounts' acc breaks
    else
        -- Close old period, start new period.
        let periodStart' = breakSubmissionTimestamp bs in
        let acc' = (periodStart, periodStart', currentCounts):acc in

        breaksToPeriods endTime periodStart' currentCounts' acc' breaks
    
    where
        combineCounts (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

        breakToCount bs = case breakSubmissionBreakType bs of
            Just BreakCorrectness     -> (1,0,0)
            Just BreakCrash           -> (0,1,0)
            Just BreakConfidentiality -> (0,0,1)
            Just BreakIntegrity       -> (0,0,1)
            Just BreakAvailability    -> (0,0,1)
            Just BreakSecurity        -> (0,0,1)
            Nothing                   -> error "breakToCount: unreachable"


    
        attackerId = breakSubmissionTeam bs


scoreOverTime :: Double -> UTCTime -> UTCTime -> Double
scoreOverTime m startTime endTime 
  | endTime `diffUTCTime` startTime <= 0 = 0
  | endTime `diffUTCTime` startTime < fromInteger bufferTime = m
  | otherwise = m * ((fromRational $ toRational $ endTime `diffUTCTime` startTime) / fromInteger bufferTime)

-- 24 hours
-- bufferTime = secondsToNominalDiffTime $ 24 * 60 * 60
bufferTime :: Integer
bufferTime = 24 * 60 * 60



breakTypeToM :: Maybe BreakType -> Double
breakTypeToM (Just BreakCorrectness)     = 25
breakTypeToM (Just BreakCrash)           = 50
breakTypeToM (Just BreakConfidentiality) = 100
breakTypeToM (Just BreakIntegrity)       = 100
breakTypeToM (Just BreakAvailability)    = 100
breakTypeToM (Just BreakSecurity)        = 100
breakTypeToM Nothing                     = error "breakTypeToM: unreachable"
        

