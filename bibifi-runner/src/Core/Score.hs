{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Core.Score (defaultScoreBuildRound, defaultScoreBreakRound, defaultScoreFixRound) where

import BuildSubmissions (buildSubmissionPassesRequiredTests)
import Core.DatabaseM
import Control.Monad
import Control.Monad.Trans.Class
import Data.Hashable
import qualified Data.HashMap as M
-- import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Set as S
import Data.Time
import qualified Database.Esqueleto as E
import Database.Persist
import Model
import PostDependencyType

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

isBreakCrash :: BreakSubmission -> Bool
isBreakCrash submission =
    let breakTypeM = breakSubmissionBreakType submission in
    breakTypeM == Just BreakCrash

defaultScoreBreakRound :: ContestId -> DatabaseM ()
defaultScoreBreakRound cId = 
    let score m (accBuilder,accBreaker) submission = 
          let submitter = breakSubmissionTeam submission in
          let target = breakSubmissionTargetTeam submission in
          let score = case breakSubmissionResult submission of
                Just BreakCorrect ->
                    if isBreakCrash submission then 
                        m
                    else
                        m/2.0
                Just BreakExploit ->
                    2*m
                _ ->
                    -- make this bool?? IO?
                    -- silentFail "this should never happen"
                    error "this should never happen"
          in
          let builderS = addScore accBuilder (target,-score) in
          let breakerS = addScore accBreaker (submitter,score) in
          ( builderS, breakerS)
    in
    do
    -- Get number of teams, m
    -- m' <- runDB $ count [TeamContestContest ==. cId]
    let m = 50 -- fromIntegral m' :: Double
    -- Select all break submissions from contest where result = bug or exploit
    submissions' <- runDB $ E.select $ E.from $ \(tc `E.InnerJoin` bs) -> do
        E.on ( tc E.^. TeamContestId E.==. bs E.^. BreakSubmissionTeam)
        E.where_ (tc E.^. TeamContestContest E.==. E.val cId
            E.&&. ((bs E.^. BreakSubmissionStatus E.==. E.val BreakTested) E.||. (bs E.^. BreakSubmissionStatus E.==. E.val BreakJudged))
            E.&&. (bs E.^. BreakSubmissionResult E.==. E.val (Just BreakCorrect) 
            E.||. bs E.^. BreakSubmissionResult E.==. E.val (Just BreakExploit)))
        return bs
    let submissions = fmap entityVal submissions'
    initialScores <- getZeroScores
    let ( builderScores, breakerScores) = foldl (score m) initialScores submissions
    -- Insert latest scores
    now <- lift getCurrentTime
    mapM_ (updateBuildBreakScore now) builderScores
    mapM_ (updateBreakBreakScore now) breakerScores

    where
        getZeroScores = do
            -- Get all teams that have made a break submission for the current contest.
            teams <- runDB $ E.select $ E.distinct $ E.from $ \(tc `E.InnerJoin` bs) -> do
                E.on ( tc E.^. TeamContestId E.==. bs E.^. BreakSubmissionTeam)
                E.where_ (tc E.^. TeamContestContest E.==. E.val cId)
                return (tc E.^. TeamContestId)
            let zeros = map (\(E.Value tcId) -> ( tcId, 0)) teams
            return ([],zeros)
            

defaultScoreFixRound :: ContestId -> DatabaseM ()
defaultScoreFixRound cId = 
    -- Iterate over a builder team's fix submissions. 
    let scoreSubmission m disqualified ((accBuilder'',accBreaker''),fixed') s' = do
          let Entity fsId fs = s'
          -- get all bugs (inner joing FixSubmissionBugs and breaksubmission) (where result == bug or exploit)
          bugs <- runDB $ E.select $ E.from $ \(b `E.InnerJoin` bs `E.InnerJoin` tc) -> do
              E.on ( tc E.^. TeamContestId E.==. bs E.^. BreakSubmissionTeam)
              E.on (b E.^. FixSubmissionBugsBugId E.==. bs E.^. BreakSubmissionId)
              E.where_ ( b E.^. FixSubmissionBugsFix E.==. E.val fsId 
                  E.&&. (bs E.^. BreakSubmissionStatus E.==. E.val BreakTested
                      E.||. bs E.^. BreakSubmissionStatus E.==. E.val BreakJudged)
                  E.&&. (bs E.^. BreakSubmissionResult E.==. E.val (Just BreakCorrect)
                  E.||. bs E.^. BreakSubmissionResult E.==. E.val (Just BreakExploit)))
              return (b,bs,tc E.^. TeamContestProfessional)
          -- foreach bug
          --  check that bugid not in disqualified or already fixed
          -- .. construct breaker team -> (bug count, exploit count, is professional)
          (breakCount,fixed) <- foldM (\(acc,fixed') (bug',break',E.Value professionalBreaker) ->
                  let (Entity _ bug) = bug' in
                  let (Entity breakId break) = break' in
                  if S.member (fixSubmissionBugsBugId bug) disqualified || S.member breakId fixed' then
                      return (acc,fixed')
                  else
                      let counts = case breakSubmissionResult break of
                            Just BreakCorrect -> 
                                if isBreakCrash break then
                                    (0,1,0,professionalBreaker)
                                else
                                    (1,0,0,professionalBreaker)
                            Just BreakExploit -> 
                                (0,0,1,professionalBreaker)
                            _ -> error "this should never happen"
                      in
                      -- Mark break submission as fixed
                      let fixed = S.insert breakId fixed' in

                      -- (liftDB g) $ runDB $ E.update $ \b -> do
                      --     E.set b [BreakSubmissionFixed E.=. E.val True]
                      --     E.where_ ( b E.^. BreakSubmissionId E.==. E.val breakId)
                      -- Update map
                      let newMap = M.insertWith (\(b1,c1,e1,p1) (b2,c2,e2,p2) -> 
                              (b1 + b2, c1 + c2, e1 + e2, p1 || p2)
                            ) (breakSubmissionTeam break) counts acc
                      in

                      return ( newMap, fixed)
                ) (M.empty,fixed') bugs

          -- Check if a breaker team hit the same bug multiple times.
          let (accBuilder',accBreaker',bC,bC',cC,cC',eC,eC') = M.foldWithKey (\k (bC,cC,eC,prof) (accBuilder',accBreaker',accBC,accBC',accCC,accCC',accEC,accEC') ->
                  let exploit = eC > 0 in
                  let crash = cC > 0 in
                  let (accBuilder,accBreaker) = 
                        if bC + eC > 1 then
                          -- Compute points to give back for hitting the same bug
                          let prevPoints = ((fromIntegral bC) * m)/2.0 + (fromIntegral cC) * m + (fromIntegral eC) * 2*m in
                          let points = if exploit then
                                  prevPoints - 2*m
                                else if crash then
                                  prevPoints - m
                                else
                                  prevPoints - m/2.0
                          in
                          -- Remove/add points to breaker/fixer
                          let accBreaker'' = addScore accBreaker' (k,-points) in
                          let accBuilder'' = addScore accBuilder' ( fixSubmissionTeam fs, points) in
                          (accBuilder'',accBreaker'')
                        else
                          (accBuilder',accBreaker')
                  in
                  -- Increment acc counters
                  case ( exploit, prof) of
                      ( True, False) ->
                          (accBuilder,accBreaker,accBC,accBC',accCC,accCC',accEC+1,accEC')
                      ( True, True) -> 
                          (accBuilder,accBreaker,accBC,accBC',accCC,accCC',accEC,accEC'+1)
                      ( False, False) ->
                          if crash then
                            (accBuilder,accBreaker,accBC,accBC',accCC+1,accCC',accEC,accEC')
                            
                          else
                            (accBuilder,accBreaker,accBC+1,accBC',accCC,accCC',accEC,accEC')
                      ( False, True) ->
                          if crash then
                            (accBuilder,accBreaker,accBC,accBC',accCC,accCC'+1,accEC,accEC')
                          else
                            (accBuilder,accBreaker,accBC,accBC'+1,accCC,accCC',accEC,accEC')
                ) (accBuilder'',accBreaker'',0,0,0,0,0,0) breakCount

          -- Compute points to give back for bugs hit by multiple teams
          let accBuilder = 
                let bugC = bC + bC' in
                let crashC = cC + cC' in
                let exploitC = eC + eC' in
                let count = bugC + exploitC + crashC in
                if count > 1 then
                  let prevPenalty = ((fromIntegral bugC) * m)/2.0 + (fromIntegral exploitC) * 2*m + (fromIntegral crashC) * m in
                  -- Check if an exploit was found. 
                  let pointsBack = if exploitC > 0 then
                          prevPenalty - 2*m
                        else if crashC > 0 then
                          prevPenalty - m
                        else
                          prevPenalty - m/2.0
                  in
                  addScore accBuilder' ( fixSubmissionTeam fs,pointsBack)
                else
                  accBuilder'

          -- Compute how to share points between breakers. 
          let accBreaker = 
                M.foldWithKey (\k (_foundBC,foundCC,foundEC,prof) acc ->
                  -- Don't penalize other breakers for professional team findings.
                  let count = if prof then
                          bC + bC' + cC + cC' + eC + eC'
                        else
                          bC + cC + eC
                  in
                  -- Just to be sure we don't divide by zero (and a slight optimisation).
                  if count > 1 then
                      let factor = - (fromIntegral (count - 1)) / (fromIntegral count) in
                      -- Check if the breaker team found an exploit.
                      let p = if foundEC > 0 then
                            2*m
                          else if foundCC > 0 then
                            m
                          else
                            m/2.0
                      in
                      addScore acc (k,factor*p)
                  else
                      acc
                ) accBreaker' breakCount

          return ((accBuilder,accBreaker), fixed)
    in
    -- Iterate over teams.
    let score m acc team' = do
          let (Entity teamId _) = team'
          -- Get all disqualified bug submissions for team
          disqualified' <- runDB $ E.select $ E.from $ \( s `E.InnerJoin` b) -> do
              E.on ( s E.^. FixSubmissionId E.==. b E.^. FixSubmissionBugsFix)
              E.where_ ( s E.^. FixSubmissionTeam E.==. E.val teamId E.&&. s E.^. FixSubmissionResult E.==. E.val (Just FixDisqualified))
              return ( b E.^. FixSubmissionBugsBugId)
          let disqualified = foldl (\acc dis' -> 
                  let E.Value dis = dis' in
                  S.insert dis acc
                ) S.empty disqualified'

          -- Select all fix submissions from contest for team where built and fixed
          submissions <- runDB $ E.select $ E.from $ \s -> do
              E.where_ (s E.^. FixSubmissionTeam E.==. E.val teamId 
                  E.&&. (s E.^. FixSubmissionStatus E.==. E.val FixBuilt
                      E.||. s E.^. FixSubmissionStatus E.==. E.val FixJudged)
                  E.&&. s E.^. FixSubmissionResult E.==. E.val (Just FixFixed))
              E.orderBy [E.asc (s E.^. FixSubmissionTimestamp)]
              return s

          (scoreMap, _fixed) <- foldM (scoreSubmission m disqualified) (acc, S.empty) submissions
          return scoreMap
    in
    do
    -- Get number of teams, m
    -- m' <- runDB $ count [TeamContestContest ==. cId]
    let m = 50 -- fromIntegral m' :: Double
    -- Produce list of teams
    teams <- runDB $ selectList [TeamContestContest ==. cId] []
    ( builderScores, breakerScores) <- foldM (score m) ([],[]) teams
    -- foreach team
    --  get all disqualified submission ids
    --  get all fix submissions for team
    --      check if disqualifed
    --      check if same team targeted same bug more than once (remove points)
    --      assign actual penalty only once (exploit vs bug)
    --      split up points between breakers


    -- Insert latest scores
    now <- lift getCurrentTime
    mapM_ (updateBuildFixScore now) builderScores
    mapM_ (updateBreakFixScore now) breakerScores

