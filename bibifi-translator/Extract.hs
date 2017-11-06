{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Extract (extract) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as Text
import Database.Persist
import PostDependencyType
import qualified System.Directory as Directory
import qualified System.FilePath as Path
import Text.CSV (printCSV)

import Common

data OutputData = OutputData {
      outputContestUrl :: Text
    , outputTeams :: [(Entity TeamContest, Team)]
    , outputUsers :: [(TeamContestId, Entity User, Maybe (Entity UserInformation))]
    , outputBuildScores :: [Entity TeamBuildScore]
    , outputBreakScores :: [Entity TeamBreakScore]
    , outputBuildSubmissions :: [Entity BuildSubmission]
    , outputBuildCoreResults :: [Entity BuildCoreResult]
    , outputBuildPerformanceResults :: [Entity BuildPerformanceResult]
    , outputBuildOptionalResults :: [Entity BuildOptionalResult]
    , outputBreakSubmissions :: [(Entity BreakSubmission, Maybe (Entity BreakJudgement))]
    , outputFixSubmissions :: [(Entity FixSubmission, Maybe (Entity FixJudgement))]
    , outputFixSubmissionBreaks :: [Entity FixSubmissionBugs]
    }
    -- TODO: BuildSubmissions, TestResults, BreakSubmissions, BreakJudgements, FixSubmissions, Adjusted scores, ... XXX

-- Extract data from contests.
extract :: Entity Contest -> [String] -> DatabaseM ()
extract _ [] = boolFail "error: no output directory specified"
extract _ [_dir] = boolFail "error: no contest url specified"
extract _ (dir:contests) = do
    -- Get data.
    dats <- mapM getData contests

    -- TODO: Filter out people who have opted out? Or do this later. XXX

    -- Output results.
    mapM_ (outputResult dir) dats

getData :: String -> DatabaseM OutputData
getData contestUrlS = do
    -- Get contest.
    let contestUrl = Text.pack contestUrlS
    (Entity contestId contest) <- getContest contestUrl

    -- Get teams.
    teams' <- getTeams contestId
    let teams = map (\(tc, (Entity _ team)) -> (tc, team)) teams'

    -- Get team members.
    members <- fmap mconcat $ mapM getTeamMembers teams'

    -- Get team scores.
    buildScores <- mconcat <$> mapM getBuildScore teams'
    breakScores <- mconcat <$> mapM getBreakScore teams'
    
    -- Get team's build submissions.
    buildSubmissions <- mconcat <$> mapM getBuildSubmissions teams

    -- Get test results.
    buildCoreResults <- mconcat <$> mapM getBuildCoreResults buildSubmissions
    buildOptionalResults <- mconcat <$> mapM getBuildOptionalResults buildSubmissions
    buildPerformanceResults <- mconcat <$> mapM getBuildPerformanceResults buildSubmissions

    -- Get break submissions.
    breakSubmissions <- mconcat <$> mapM getBreakSubmissions teams

    -- Get fix submissions.
    fixSubmissions <- mconcat <$> mapM getFixSubmissions teams

    -- Get fix submission breaks.
    fixSubmissionBreaks <- mconcat <$> mapM getFixSubmissionBreaks fixSubmissions

    return $ OutputData contestUrl teams members buildScores breakScores buildSubmissions buildCoreResults buildPerformanceResults buildOptionalResults breakSubmissions fixSubmissions fixSubmissionBreaks

    where
        getBuildScore (Entity tcId _, _) = do
            runDB $ selectList [TeamBuildScoreTeam ==. tcId] [Desc TeamBuildScoreTimestamp, LimitTo 1]

        getBreakScore (Entity tcId _, _) = do
            runDB $ selectList [TeamBreakScoreTeam ==. tcId] [Desc TeamBreakScoreTimestamp, LimitTo 1]

        getFixSubmissionBreaks (Entity fId _, _) = 
            runDB $ selectList [FixSubmissionBugsFix ==. fId] []

        getFixSubmissions (Entity tcId _, _) = 
            runDB $ selectList [FixSubmissionTeam ==. tcId] [] >>= (mapM $ \fs@(Entity fsId _) -> do
                    judgement <- selectFirst [FixJudgementSubmission ==. fsId] []
                    return (fs, judgement)
                )


        getBreakSubmissions (Entity tcId _, _) = 
            runDB $ selectList [BreakSubmissionTeam ==. tcId] [] >>= (mapM $ \bs@(Entity bsId _) -> do
                    judgement <- selectFirst [BreakJudgementSubmission ==. bsId] []
                    return (bs, judgement)
                )


        getBuildCoreResults (Entity bsId _) = runDB $ selectList [BuildCoreResultSubmission ==. bsId] []
        getBuildOptionalResults (Entity bsId _) = runDB $ selectList [BuildOptionalResultSubmission ==. bsId] []
        getBuildPerformanceResults (Entity bsId _) = runDB $ selectList [BuildPerformanceResultSubmission ==. bsId] []


        getContest contestUrl = do
            contestM <- runDB $ getBy $ UniqueContest contestUrl
            maybe 
                (boolFail $ "error: invalid contest url: " ++ contestUrlS)
                return contestM


        getTeams contestId = do
            teams <- runDB $ selectList [TeamContestContest ==. contestId] [Asc TeamContestId]
            mapM (\tc@(Entity tcId teamContest) -> do
                    let teamId = teamContestTeam teamContest
                    team <- do
                        teamM <- runDB $ get $ teamContestTeam teamContest
                        maybe 
                            (boolFail $ "error: could not find team contest with id: " ++ show tcId)
                            (return . Entity teamId) teamM

                    return (tc, team)
                ) teams


        getTeamMembers ((Entity tcId teamContest),(Entity teamId team)) = do
            members' <- do
                members <- runDB $ selectList [TeamMemberTeam ==. teamId] [Asc TeamMemberUser]
                return $ map (teamMemberUser . entityVal) members

            let members = (teamLeader team):members'
            mapM (getTeamMember tcId) members


        getTeamMember tcId userId = do
            -- Get user.
            user <- do
                userM <- runDB $ get userId
                maybe (boolFail $ "error: could not find user with id: " ++ show userId) (return . Entity userId) userM

            -- Get user information.
            userInformationM <- do
                infoM <- runDB $ getBy $ UniqueUserInformation userId
                -- maybe (liftIO $ putStrLn $ "error: could not find user info for user id: " ++ show userId) return infoM
                case infoM of
                    Nothing ->
                        liftIO $ putStrLn $ "error: could not find user info for user id: " ++ show userId
                    Just _ ->
                        return ()
                return infoM
            
            return (tcId, user, userInformationM)


        getBuildSubmissions (Entity tcId _, _) = 
            runDB $ selectList [BuildSubmissionTeam ==. tcId] []


outputResult :: String -> OutputData -> DatabaseM ()
outputResult outputDir OutputData{..} = liftIO $ do
    -- Make output directory.
    let dir = Path.combine outputDir $ Text.unpack outputContestUrl
    Directory.createDirectoryIfMissing True dir

    let teams = ("teams", teamsToCSV outputTeams)

    let users = ("users", usersToCSV outputUsers)

    let buildSubmissions = ( "build_submissions", buildSubmissionsToCSV outputBuildSubmissions)

    let buildCoreResults = ( "build_core_results", buildCoreResultsToCSV outputBuildCoreResults)

    let buildPerformanceResults = ( "build_performance_results", buildPerformanceResultsToCSV outputBuildPerformanceResults)

    let buildOptionalResults = ( "build_optional_results", buildOptionalResultsToCSV outputBuildOptionalResults)

    let breakSubmissions = ( "break_submissions", breakSubmissionsToCSV outputBreakSubmissions)

    let fixSubmissions = ( "fix_submissions", fixSubmissionsToCSV outputFixSubmissions)

    let fixSubmissionBreaks = ( "fix_submission_breaks", fixSubmissionBreaksToCSV outputFixSubmissionBreaks)
    let buildScores = ("build_scores", buildScoresToCSV outputBuildScores)
    let breakScores = ("break_scores", breakScoresToCSV outputBreakScores)

    mapM_ (saveCSV dir) [teams, users, buildSubmissions, buildCoreResults, buildPerformanceResults, buildOptionalResults, breakSubmissions, fixSubmissions, buildScores, breakScores, fixSubmissionBreaks]
    

    where
        saveCSV dir (filename, csv) = do
            let file = dir `Path.combine` filename `Path.addExtension` "csv"
            writeFile file $ printCSV csv
        
        fst3 (a,_,_) = a
        snd3 (_,b,_) = b
        thd3 (_,_,c) = c

        toCSVHelper col xs = -- zip instead?
            let (header, fs) = unzip col in
            (header:) (map (\x -> map (\f -> f x) fs) xs)

        buildSubmissionsToCSV = toCSVHelper 
            [ ("id",         toCSV . entityKey)
            , ("team",       toCSV . buildSubmissionTeam . entityVal)
            , ("timestamp",  toCSV . buildSubmissionTimestamp . entityVal)
            , ("commitHash", toCSV . buildSubmissionCommitHash . entityVal)
            , ("status",     toCSV . buildSubmissionStatus . entityVal)
            ]

        fixSubmissionBreaksToCSV = toCSVHelper
            [ ("fixid", toCSV . fixSubmissionBugsFix . entityVal)
            , ("breakId", toCSV . fixSubmissionBugsBugId . entityVal)
            ]

        teamsToCSV = toCSVHelper 
            [ ("id",           toCSV . entityKey . fst)
            , ("languages",    toCSV . teamContestLanguages . entityVal . fst)
            , ("professional", toCSV . teamContestProfessional . entityVal . fst)
            , ("team",         toCSV . teamName . snd)
            ]

        buildScoresToCSV = toCSVHelper 
            [ ("team", toCSV . teamBuildScoreTeam . entityVal)
            , ("build_round", toCSV . teamBuildScoreBuildScore . entityVal)
            , ("break_round", toCSV . teamBuildScoreBreakScore . entityVal)
            , ("fix_round", toCSV . teamBuildScoreFixScore . entityVal)
            ]

        breakScoresToCSV = toCSVHelper 
            [ ("team", toCSV . teamBreakScoreTeam . entityVal)
            , ("build_round", toCSV . teamBreakScoreBuildScore . entityVal)
            , ("break_round", toCSV . teamBreakScoreBreakScore . entityVal)
            , ("fix_round", toCSV . teamBreakScoreFixScore . entityVal)
            ]

        buildCoreResultsToCSV = toCSVHelper
            [ ("submission", toCSV . buildCoreResultSubmission . entityVal)
            , ("testid", toCSV . buildCoreResultTest . entityVal)
            , ("pass", toCSV . buildCoreResultPass . entityVal)
            , ("message", toCSV . buildCoreResultMessage . entityVal)
            ]

        buildPerformanceResultsToCSV = toCSVHelper
            [ ("submission", toCSV . buildPerformanceResultSubmission . entityVal)
            , ("testid", toCSV . buildPerformanceResultTest . entityVal)
            , ("performance", toCSV . buildPerformanceResultTime . entityVal)
            , ("message", toCSV . buildPerformanceResultMessage . entityVal)
            ]

        buildOptionalResultsToCSV = toCSVHelper
            [ ("submission", toCSV . buildOptionalResultSubmission . entityVal)
            , ("testid", toCSV . buildOptionalResultTest . entityVal)
            , ("pass", toCSV . buildOptionalResultPass . entityVal)
            , ("message", toCSV . buildOptionalResultMessage . entityVal)
            ]
            
        breakSubmissionsToCSV = toCSVHelper
            [ ("id", toCSV . entityKey . fst)
            , ("team", toCSV . breakSubmissionTeam . entityVal . fst)
            , ("target_team", toCSV . breakSubmissionTargetTeam . entityVal . fst)
            , ("timestamp", toCSV . breakSubmissionTimestamp . entityVal . fst)
            , ("commit", toCSV . breakSubmissionCommitHash . entityVal . fst)
            , ("status", toCSV . breakSubmissionStatus . entityVal . fst)
            , ("result", toCSV . breakSubmissionResult . entityVal . fst)
            , ("name", toCSV . breakSubmissionName . entityVal . fst)
            , ("type", toCSV . breakSubmissionBreakType . entityVal . fst)
            , ("message", toCSV . breakSubmissionMessage . entityVal . fst)
            , ("judgeid", toCSV . fmap (breakJudgementJudge . entityVal) . snd)
            , ("judge_accepted", toCSV . fmap (breakJudgementRuling . entityVal) . snd)
            -- , ("judge_comments", toCSV . breakSubmission . entityVal)
            ]
        fixSubmissionsToCSV = toCSVHelper
            [ ("id", toCSV . entityKey . fst)
            , ("team", toCSV . fixSubmissionTeam . entityVal . fst)
            , ("timestamp", toCSV . fixSubmissionTimestamp . entityVal . fst)
            , ("commit", toCSV . fixSubmissionCommitHash . entityVal . fst)
            , ("status", toCSV . fixSubmissionStatus . entityVal . fst)
            , ("result", toCSV . fixSubmissionResult . entityVal . fst)
            , ("name", toCSV . fixSubmissionName . entityVal . fst)
            , ("message", toCSV . fixSubmissionMessage . entityVal . fst)
            , ("judgeid", toCSV . fmap (fixJudgementJudge . entityVal) . snd)
            , ("judge_accepted", toCSV . fmap (fixJudgementRuling . entityVal) . snd)
            ]

        -- TeamContestId, Entity User, Maybe (Entity UserInformation)
        usersToCSV = toCSVHelper
            [ ("id",                   toCSV . entityKey . snd3)
            , ("team",                 toCSV . fst3)
            , ("user",                 toCSV . userIdent . entityVal . snd3)
            , ("email",                toCSV . userEmail . entityVal . snd3)
            , ("school",               toCSV . fmap (userInformationSchool . entityVal) . thd3)
            , ("major",                toCSV . fmap (userInformationMajor . entityVal) . thd3)
            , ("minor",                toCSV . fmap (userInformationMinor . entityVal) . thd3)
            , ("degreesHeld",          toCSV . fmap (userInformationDegreesHeld . entityVal) . thd3)
            , ("degree",               toCSV . fmap (userInformationDegree . entityVal) . thd3)
            , ("yearsInProgram",       toCSV . fmap (userInformationYearsInProgram . entityVal) . thd3)
            , ("yearsOfExperience",    toCSV . fmap (userInformationYearsOfExperience . entityVal) . thd3)
            , ("languages",            toCSV . fmap (userInformationLanguages . entityVal) . thd3)
            , ("favoriteLanguages",    toCSV . fmap (userInformationFavoriteLanguages . entityVal) . thd3)
            , ("yearsOfWork",          toCSV . fmap (userInformationYearsOfWork . entityVal) . thd3)
            , ("experienceClass",      toCSV . fmap (userInformationExperienceClass . entityVal) . thd3)
            , ("experiencePersonal",   toCSV . fmap (userInformationExperiencePersonal . entityVal) . thd3)
            , ("experienceInternship", toCSV . fmap (userInformationExperienceInternship . entityVal) . thd3)
            , ("experienceJob",        toCSV . fmap (userInformationExperienceJob . entityVal) . thd3)
            , ("securityTraining",     toCSV . fmap (userInformationSecurityTraining . entityVal) . thd3)
            , ("securityExperience",   toCSV . fmap (userInformationSecurityExperience . entityVal) . thd3)
            , ("softwareEngineering",  toCSV . fmap (userInformationSoftwareEngineering . entityVal) . thd3)
            , ("securityClass",        toCSV . fmap (userInformationSecurityClass . entityVal) . thd3)
            , ("previousContest",      toCSV . fmap (userInformationPreviousContest . entityVal) . thd3)
            , ("age",                  toCSV . fmap (userInformationAge . entityVal) . thd3)
            , ("nationality",          toCSV . fmap (userInformationNationality . entityVal) . thd3)
            , ("gender",               toCSV . fmap (userInformationGender . entityVal) . thd3)
            , ("agreeToParticipate",   toCSV . fmap (userInformationAgreeToParticipate . entityVal) . thd3)
            , ("graduationYear",       toCSV . fmap (userInformationGraduationYear . entityVal) . thd3)
            , ("programmerRating",     toCSV . fmap (userInformationProgrammerRating . entityVal) . thd3)
            , ("attackerRating",       toCSV . fmap (userInformationAttackerRating . entityVal) . thd3)
            , ("language",             toCSV . fmap (userInformationLanguage . entityVal) . thd3)
            , ("timezone",             toCSV . fmap (userInformationTimezone . entityVal) . thd3)
            ]

class ToCSV a where
    toCSV :: a -> String

instance ToCSV Bool where
    toCSV True = "True"
    toCSV False = "False"

instance ToCSV Text where
    toCSV = Text.unpack

instance PersistEntity a => ToCSV (Key a) where
    toCSV = toCSV . keyToInt

instance ToCSV Double where
    toCSV = show

instance ToCSV Int where
    toCSV = show

instance ToCSV Int64 where
    toCSV = show

instance ToCSV BreakType where
    toCSV = show

instance ToCSV BreakSubmissionResult where
    toCSV = show

instance ToCSV BreakSubmissionStatus where
    toCSV = show

instance ToCSV a => ToCSV (Maybe a) where
    toCSV Nothing = ""
    toCSV (Just a) = toCSV a

instance ToCSV UTCTime where
    toCSV = show

instance ToCSV String where
    toCSV = id
    
instance ToCSV FixSubmissionResult where
    toCSV = show
    
instance ToCSV FixSubmissionStatus where
    toCSV = show
    
instance ToCSV BuildSubmissionStatus where
    toCSV = show
    


