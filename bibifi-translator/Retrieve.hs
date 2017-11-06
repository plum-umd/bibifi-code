module Retrieve where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Database.Esqueleto as E
import Database.Persist
import Score
import System.IO (hPutStrLn, stderr)
import Text.Printf

import Common
import BuildSubmissions
import PostDependencyType

retrieve :: [String] -> DatabaseM ()
retrieve args' =
    case args' of 
        [] ->
            usage
        cmd':args ->
            case lookup (fmap C.toLower cmd') dispatch of 
                Nothing ->
                    usage
                Just cmd ->
                    cmd args

dispatch :: [(String, [String] -> DatabaseM ())]  
dispatch = [ ( "teams", teams), ( "tests", tests), ( "resumes", resumes), ( "missing", missing), ("grades", grades), ("scores", scores), ("removeteam", removeTeams), ("emails", emails)]

usage :: MonadIO m => m ()
usage = silentFail $ usageDispatch "RETRIEVE" dispatch

data RetTeam = RetTeam { teamId :: Key TeamContest, gitURL :: T.Text, qualifiedBuilder :: Bool}
    deriving (Show)

instance ToJSON RetTeam where
    toJSON (RetTeam teamId gitURL qualifiedBuilder) = object 
        [ "teamId" .= teamId, "gitURL" .= gitURL, "qualifiedBuilder" .= qualifiedBuilder]

newtype EntityPerformanceTest = EntityPerformanceTest {performanceT :: Entity ContestPerformanceTest}
instance ToJSON EntityPerformanceTest where
    toJSON (EntityPerformanceTest (Entity id (ContestPerformanceTest _ name inputFile outputFile testScript optional))) = object [
        "type" .= ("Performance" :: T.Text),
        "name" .= name,
        "input" .= inputFile,
        "output" .= outputFile,
        "script" .= testScript,
        "testid" .= id,
        "optional" .= optional
        ]

newtype EntityOptionalTest = EntityOptionalTest { optionalT :: Entity ContestOptionalTest}
instance ToJSON EntityOptionalTest where
    toJSON (EntityOptionalTest (Entity id (ContestOptionalTest _ name inputFile outputFile testScript))) = object [
        "type" .= ("Optional" :: T.Text),
        "name" .= name,
        "input" .= inputFile,
        "output" .= outputFile,
        "script" .= testScript,
        "testid" .= id
        ]

newtype EntityCoreTest = EntityCoreTest { coreT :: Entity ContestCoreTest}
instance ToJSON (EntityCoreTest) where
    toJSON (EntityCoreTest (Entity id (ContestCoreTest _ name inputFile outputFile testScript))) = object [
        "type" .= ("Core" :: T.Text),
        "name" .= name,
        "input" .= inputFile,
        "output" .= outputFile,
        "script" .= testScript,
        "testid" .= id
        ]

teams :: [String] -> DatabaseM ()
teams args = 
    case args of 
        [] ->
            do
            Entity c _ <- getContest
            let toTeam (Entity teamId team) = runDB $ do
                  bss <- getLatestBuildSubmissions c $ \tc bs -> do
                      E.where_ (tc E.^. TeamContestId E.==. E.val teamId)
                      return ( bs E.^. BuildSubmissionId)
                  qualified <- case bss of
                    (E.Value bsId):_ ->
                        buildSubmissionPassesRequiredTests c bsId
                    _ ->
                        return False
                  return $ RetTeam teamId (teamContestGitUrl team) qualified
            teams <- runDB $ selectList [TeamContestContest ==. c] []
            teams' <- mapM toTeam teams
            liftIO $ BS.putStrLn $ encode $ toJSON teams'
        _ ->
            silentFail "error: too many arguments"

tests :: [String] -> DatabaseM ()
tests args' = case args' of 
    [] ->
        do
        Entity c _ <- getContest
        coreTests' <- runDB $ selectList [ContestCoreTestContest ==. c] []
        let coreTests = fmap (toJSON . EntityCoreTest) coreTests'
        performanceTests' <- runDB $ selectList [ContestPerformanceTestContest ==. c] []
        let performanceTests = fmap (toJSON . EntityPerformanceTest) performanceTests'
        optionalTests' <- runDB $ selectList [ContestOptionalTestContest ==. c] []
        let optionalTests = fmap (toJSON . EntityOptionalTest) optionalTests'
        liftIO $ BS.putStrLn $ encode $ coreTests ++ performanceTests ++ optionalTests
    _ -> 
        silentFail "error: too many arguments"

grades :: [String] -> DatabaseM ()
grades [] = do
    Entity cId _ <- getContest
    teams <- runDB $ selectList [TeamContestContest ==. cId] []
    mapM_ gradeTeam teams

    where
        gradeTeam (Entity tcId tc) = do
            -- Get build score.
            buildM <- runDB $ selectFirst [TeamBuildScoreTeam ==. tcId] [Desc TeamBuildScoreId]
            let buildScore = case buildM of
                  Nothing ->
                    notQualifiedScore
                  Just (Entity _buildId build) ->
                    case (teamBuildScoreBuildScore build, teamBuildScoreBreakScore build, teamBuildScoreFixScore build) of
                    (Nothing, Nothing, Nothing) -> 
                        notQualifiedScore
                    (builds, breaks, fixs) -> 
                        sum $ map maybeToNum $ [builds, breaks, fixs]

            -- Get break score.
            breakM <- runDB $ selectFirst [TeamBreakScoreTeam ==. tcId] [Desc TeamBreakScoreId]
            let breakScore = case breakM of
                  Nothing ->
                    0
                  Just (Entity _breakId break) ->
                    sum $ map maybeToNum $ [teamBreakScoreBuildScore break, teamBreakScoreBreakScore break, teamBreakScoreFixScore break]

            -- Get number of passed core tests.
            corePassedC <- do
                submissionM <- runDB $ selectFirst [BuildSubmissionTeam ==. tcId] [Desc BuildSubmissionId]
                case submissionM of
                    Nothing ->
                        return 0
                    Just (Entity sId _) ->
                        runDB $ count [BuildCoreResultSubmission ==. sId, BuildCoreResultPass ==. True]

            -- Get number of accepted fix submissions.
            fixes <- runDB $ selectList [FixSubmissionTeam ==. tcId, FixSubmissionResult ==. Just FixFixed] []
            let fixC = L.length fixes

            -- Get number of unfixed breaks.
            remainingBreakC <- do
                fixedBugC <- fmap (L.length . L.concat) $  mapM (\(Entity fId _) -> runDB $ selectList [FixSubmissionBugsFix ==. fId] []) fixes

                breakBugC <- runDB $ count [BreakSubmissionTargetTeam ==. tcId, FilterOr [BreakSubmissionResult ==. Just BreakCorrect, BreakSubmissionResult ==. Just BreakExploit]]



                return $ breakBugC - fixedBugC

            
            -- Grade users.
            teamM <- runDB $ get $ teamContestTeam tc
            case teamM of
                Nothing ->
                    error "This shouldn't happen. Team not found."
                Just team -> do
                    gradeUser buildScore breakScore (teamName team) corePassedC fixC remainingBreakC $ teamLeader team
                    members <- runDB $ selectList [TeamMemberTeam ==. teamContestTeam tc] []
                    mapM_ (gradeUser buildScore breakScore (teamName team) corePassedC fixC remainingBreakC . teamMemberUser . entityVal) members


        gradeUser buildScore breakScore team corePassedC fixC remainingBreakC userId = do
            (ident, email) <- do
                userM <- runDB $ get userId
                case userM of
                    Nothing ->
                        error "This shouldn't happen. User not found."
                    Just user -> do
                        return (userIdent user, userEmail user)
            couseraId <- do
                courseraM <- runDB $ getBy $ UniqueCourseraUser userId
                case courseraM of
                    Nothing -> do
                        liftIO $ hPutStrLn stderr $ "User doesn't have a coursera ID: " ++ T.unpack ident ++ " (" ++ show (keyToInt userId) ++ ")"
                        return "MISSING"
                    Just (Entity _ coursera) -> 
                        return $ courseraUserCourseraId coursera

            liftIO $ putStrLn $ T.unpack ident ++ ", " ++ T.unpack couseraId ++ ", " ++ T.unpack team ++ ", " ++ show buildScore ++ ", " ++ show breakScore ++ ", " ++ T.unpack email ++ ", " ++ show corePassedC ++ ", " ++ show fixC  ++ ", " ++ show remainingBreakC

        notQualifiedScore = -100000

        maybeToNum Nothing = 0
        maybeToNum (Just x) = x
grades _ = silentFail "error: too many arguments"

missing :: [String] -> DatabaseM ()
missing args' = case args' of
    [] -> do
        Entity cId _ <- getContest
        -- Get list of teams. 
        teams <- runDB $ E.select $ E.from $ \(E.InnerJoin t tc) -> do
            E.on (t E.^. TeamId E.==. tc E.^. TeamContestTeam)
            E.where_ ( tc E.^. TeamContestContest E.==. E.val cId E.&&. tc E.^. TeamContestProfessional E.==. E.val False)
            return ( tc E.^. TeamContestId, t E.^. TeamId)--, t E.^. TeamName)
        mapM_ (\(E.Value tcId, E.Value tId) -> do
            -- Filter out those who didn't make any submissions.
            buildC <- runDB $ count [BuildSubmissionTeam ==. tcId]
            breakC <- runDB $ count [BreakSubmissionTeam ==. tcId]
            when ( buildC + breakC /= 0) $ do
                -- Check that all team members have a consent form and resume.
                members <- runDB $ getTeamMembers tId
                -- Print emails of those that don't.
                mapM_ (\uId -> do
                    userM <- runDB $ get uId
                    case userM of 
                        Nothing ->
                            silentFail "error: could not find user"
                        Just user -> 
                            when ( (userResume user) == Nothing || (userConsentForm user) == Nothing) $
                                liftIO $ printf "%s, " $ T.unpack $ userEmail user
                  ) members
          ) teams
        liftIO $ printf "\n"
    _ ->
        silentFail "error: too many arguments"
    
    where getTeamMembers tId = do
            teamM <- get tId
            case teamM of
                Nothing ->
                    return []
                Just team -> do
                    members <- selectList [TeamMemberTeam ==. tId] []
                    return $ (teamLeader team):(map (\(Entity _ (TeamMember _ uId)) -> uId) members)

resumes :: [String] -> DatabaseM ()
resumes args' = case args' of
    [] -> do
        Entity cId _ <- getContest
        users <- runDB $ E.select $ E.from $ \(E.InnerJoin u ui) -> do
            E.on (u E.^. UserId E.==. ui E.^. UserInformationUser)
            E.where_ ( ui E.^. UserInformationResumePermission E.==. E.val True 
                E.&&. (E.not_ (E.isNothing (u E.^. UserResume))))
            return (u E.^. UserId, u E.^. UserResume)
        mapM_ (\(E.Value uId, E.Value (Just rId)) -> do
            -- Get team for this contest.
            tcId' <- runDB $ E.select $ E.from $ \( t `E.LeftOuterJoin` tm, tc) -> do
                E.on ( t E.^. TeamId E.==. tm E.^. TeamMemberTeam)
                E.where_ ( tc E.^. TeamContestTeam E.==. t E.^. TeamId E.&&. ( t E.^. TeamLeader E.==. E.val uId E.||. tm E.^. TeamMemberUser E.==. E.val uId) E.&&. tc E.^. TeamContestContest E.==. E.val cId)
                E.limit 1
                return ( tc E.^. TeamContestId, t E.^. TeamName)
            case tcId' of
                [(E.Value tcId, E.Value teamName)] -> do
                    -- Get team_contest's latest scores.
                    build <- do
                        buildM <- runDB $ selectFirst [TeamBuildScoreTeam ==. tcId] [Desc TeamBuildScoreTimestamp]
                        case buildM of 
                            Nothing -> 
                                return "-"
                            Just (Entity _ (TeamBuildScore _ buildBuild buildBreak buildFix _)) ->
                                return $ show $ plusM (Just (plusM buildBuild buildBreak)) buildFix

                    break <- do
                        breakM <- runDB $ selectFirst [TeamBreakScoreTeam ==. tcId] [Desc TeamBreakScoreTimestamp]
                        case breakM of 
                            Nothing -> 
                                return "-"
                            Just (Entity _ (TeamBreakScore _ breakBuild breakBreak breakFix _)) -> 
                                return $ show $ plusM breakBreak $ Just $ plusM breakFix breakBuild

                    -- Print the results.
                    liftIO $ printf "%d, %s, %s, %s\n" (keyToInt rId) (T.unpack teamName) build break
                [] ->
                    -- TODO: Don't print out for people who didn't sign up for this contest???
                    -- return ()
                    liftIO $ printf "%d, -, -, -\n" (keyToInt rId)
                _ ->
                    silentFail "error: looking up user's team"
          ) users

    _ ->
        silentFail "error: too many arguments"
            
plusM :: (Fractional a) => Maybe a -> Maybe a -> a
plusM a b = case (a, b) of
    (Just a', Just b') -> a' + b'
    (Just a', Nothing) -> a'
    (Nothing, Just b') -> b'
    (Nothing, Nothing) -> 0.0

scores :: [String] -> DatabaseM ()
scores args = case args of
    [] -> do
        Entity cId _ <- getContest
        -- let cId = either (error "fail") id $ keyFromValues [ PersistInt64 5]
        teams <- runDB $ E.select $ E.from $ \(E.InnerJoin tc team) -> do
            E.on (tc E.^. TeamContestTeam E.==. team E.^. TeamId)
            E.where_ (tc E.^. TeamContestContest E.==. E.val cId)
            return (tc E.^. TeamContestId, (team E.^. TeamName, tc E.^. TeamContestLanguages))
        let teamIds = fmap (E.unValue . fst) teams
        scores <- runDB $ E.select $ E.from $ \s -> do
            E.where_ (s E.^. TeamBuildScoreTeam `E.in_` E.valList teamIds)
            E.orderBy [E.asc (s E.^. TeamBuildScoreTimestamp)]
            return s
        mapM_ (\(Entity _sId (TeamBuildScore tcId buildS breakS fixS time)) ->
                let score = plusM buildS $ Just $ plusM breakS fixS in
                case L.lookup (E.Value tcId) teams of
                    Just ( E.Value name', E.Value languages') ->
                        let name = T.unpack name' in
                        let languages = T.unpack languages' in
                        let timestamp = round $ utcTimeToPOSIXSeconds time :: Int in
                        liftIO $ printf "%s;; %d;; %s;; %d;; %g;;\n" name (keyToInt tcId) languages timestamp score

                    Nothing ->
                        silentFail "error: should be unreachable"
            ) scores

    _ -> 
        silentFail "error: too many arguments"

removeTeams :: [String] -> DatabaseM ()
removeTeams [] = do
    -- Rescore break and fix rounds. 
    Entity cId _ <- getContest
    rescoreBreakRound cId
    rescoreFixRound cId
removeTeams (teamid:rest) = do
    removeTeam teamid
    removeTeams rest

    where
        removeTeam teamid' = runDB $ do
            -- Get all breaks.
            let teamid = either (error $ "invalid teamid: " ++ teamid') id $ keyFromValues $ [PersistInt64 $ read teamid']
            breaks <- selectList [BreakSubmissionTeam ==. teamid] []

            -- Delete all breaks. 
            mapM_ removeBreak breaks

        removeBreak (Entity breakId _) = do
            -- Delete all fix_bugs against these breaks. 
            fixBugs <- selectList [FixSubmissionBugsBugId ==. breakId] []
            mapM_ (delete . entityKey) fixBugs

            -- Delete disputes.
            disputes <- selectList [BreakDisputeBreak ==. breakId] []
            mapM_ (delete . entityKey) disputes

            -- Delete judgements. 
            judgements <- selectList [BreakJudgementSubmission ==. breakId] []
            mapM_ (delete . entityKey) judgements

            -- Delete break.
            delete breakId

emails :: [String] -> DatabaseM ()
emails [] = do
    -- Rescore break and fix rounds. 
    Entity cId _ <- getContest
    teamContests <- runDB $ selectList [TeamContestContest ==. cId] []
    runDB $ mapM_ (\tcE@(Entity tcId tc) -> do
            let tId = teamContestTeam tc
            (Just t) <- get tId
            members <- fmap (fmap $ teamMemberUser . entityVal) (selectList [TeamMemberTeam ==. tId] [])
            mapM_ (printEmail tcId t) (teamLeader t : members)
        ) teamContests

    where
        printEmail tcId t userId = do
            (Just user) <- get userId

            liftIO $ putStrLn $ (show $ keyToInt tcId) ++ ", " ++ (T.unpack $ teamName t) ++ ", " ++ (T.unpack $ userIdent user) ++ ", " ++ (T.unpack $ userEmail user)

            

-- participants :: Global -> [String] -> IO ()
-- participants g _ =
--     --let toParticipants acc tId = do
--     --      -- select u.email from user as u, team as t, team_member as tm where (t.id == (teamContestTeam teamContest) && (u.id == t.leader || (tm.team == t.id && u.id == tm.user)))
--     --      emails <- (liftDB g) $ runDB $ E.select $ E.from $ \( t, tm, u) -> do
--     --        E.where_ ( t E.^. TeamId E.==. tId
--     --            E.&&. ( u E.^. UserId E.==. t E.^. TeamLeader E.||.
--     --                (tm E.^. TeamMemberTeam E.==. t E.^. TeamId
--     --                E.&&. u E.^. UserId E.==. tm E.^. TeamMemberUser)))
--     --        return $ u E.^. UserEmail
--     --      -- rev_append
--     --      return $ foldl (\ acc email -> email:acc) emails acc
--     --in
--     do
--     c <- getContest g
--     --teams <- (liftDB g) $ runDB $ selectList [TeamContestContest ==. c] []
--     teams <- (liftDB g) $ runDB $ E.select $ E.from $ \tc -> do
--         E.where_ (tc E.^. TeamContestContest E.==. c)
--         -- return $ tc E.^. TeamContestTeam
--         return tc
--     return ()
--     --participants <- foldM toParticipants [] teams
--     --putStrLn $ show participants

