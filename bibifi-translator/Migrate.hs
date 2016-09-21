{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Migrate a team_contest to a different db. 

module Migrate where

import Control.Monad.Trans.Class (lift)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Time.Format as Time
import Database.Persist
import PostDependencyType
import qualified System.Locale as Local

import Common

-- ID of target contest we're migrating too.
targetContest = "7"

-- Mapping from one db's test ids to the other
coreTestMapping = [
      (7, "35")
    , (8, "36")
    , (9, "37")
    , (10, "38")
    , (11, "39")
    , (12, "40")
    , (1, "41")
    , (2, "42")
    , (3, "43")
    , (4, "44")
    , (5, "45")
    , (6, "46")
    ]

performanceTestMapping = [
      (1, "36")
    , (2, "37")
    , (3, "38")
    , (4, "39")
    , (5, "40")
    , (6, "41")
    , (7, "42")
    , (8, "43")
    , (9, "44")
    , (10, "45")
    ]

migrate :: [String] -> DatabaseM ()
migrate args = do
    let teamIds = map toKey args
    printSQL "BEGIN"
    print "DO $$"
    declared <- mapM declareTeamContest teamIds
    print "BEGIN"
    mapM_ migrate declared
    printSQL "END $$"
    printSQL "COMMIT"

    where
        declareTeamContest tcId = do
            -- Get team contest.
            (Just tc) <- runDB $ get tcId

            -- Get team. 
            let tId = teamContestTeam tc
            (Just t) <- runDB $ get tId

            -- Get team leader. 
            let leaderId = teamLeader t
            (Just leader) <- runDB $ get leaderId

            -- Get team members. 
            tms <- do
                tms <- runDB $ selectList [TeamMemberTeam ==. tId] []
                mapM (\ (Entity _ tm) -> do
                    let uId = teamMemberUser tm
                    (Just u) <- runDB $ get uId
                    return ( uId, u)
                  ) tms

            -- Get build submissions.
            bs <- runDB $ do
                bs <- selectList [BuildSubmissionTeam ==. tcId] []
                mapM (\bs@(Entity bsId _) -> do
                    -- Get build core, performance, and optional results.
                    cores <- selectList [BuildCoreResultSubmission ==. bsId] []
                    performances <- selectList [BuildPerformanceResultSubmission ==. bsId] []
                    optionals <- selectList [BuildOptionalResultSubmission ==. bsId] []
                    return (bs, cores, performances, optionals)
                  ) bs

            -- Get scores.
            buildScores <- runDB $ selectList [TeamBuildScoreTeam ==. tcId] []
            breakScores <- runDB $ selectList [TeamBreakScoreTeam ==. tcId] []

            -- Declare vars.
            printSQLVar leaderId
            printSQLVar tId
            printSQLVar tcId
            mapM_ (printSQLVar . fst) tms
            mapM_ (\((Entity bsId _), _, _, _) -> printSQLVar bsId) bs

            return (tcId, tc, tId, t, leaderId, leader, tms, bs, buildScores, breakScores)

        migrate (tcId, tc, tId, t, leaderId, leader, tms, bs, buildScores, breakScores) = do

            -- Migrate the team members.
            migrateUser (leaderId, leader)
            mapM_ migrateUser tms
            migrateUserInfo (leaderId, leader)
            mapM_ migrateUserInfo tms
            
            -- Migrate the team.
            migrateTeam (tId, t, leaderId)
            mapM_ (migrateTeamMember tId) tms

            -- Migrate the team contest.
            migrateTeamContest (tcId, tc, tId)
            
            -- Migrate their build submissions.
            mapM_ (migrateBuildSubmission tcId) bs

            -- Migrate their scores.
            mapM_ (migrateBuildScore tcId) buildScores
            mapM_ (migrateBreakScore tcId) breakScores

        print = lift . putStrLn
        printSQL s = print (s ++ ";")
        printSQLVar :: (ToSQLVariable i) => i -> DatabaseM ()
        printSQLVar i = printSQL $ "DECLARE " ++ toSQLVariable i ++ " bigint"

        migrateUser (uId, u) =
            let ident = Text.unpack $ userIdent u in
            let pass = Text.unpack $ userPassword u in
            let salt = Text.unpack $ userSalt u in
            let email = Text.unpack $ userEmail u in
            printSQL $ "INSERT INTO \"user\" (ident, password, salt, email) VALUES ( '" ++ ident ++ "', '" ++ pass ++ "', '" ++ salt ++  "', '" ++ email ++ "') RETURNING id INTO " ++ toSQLVariable uId

        sqlText t = "'" ++ Text.unpack (Text.replace "'" "''" t) ++ "'"
        sqlInt :: Int -> String
        sqlInt = show
        sqlDouble = show

        sqlBool True = "'t'"
        sqlBool False = "'f'"

        sqlMaybe _ Nothing = "null"
        sqlMaybe f (Just x) = f x

        sqlTime = Time.formatTime Time.defaultTimeLocale "'%Y-%m-%d %H:%M:%S'"

        sqlStatus s = case s of
            BuildPullFail -> "'BuildPullFail'"
            BuildPending -> "'BuildPending'"
            BuildBuilding -> "'BuildBuilding'"
            BuildBuildFail -> "'BuildBuildFail'"
            BuildBuilt -> "'BuildBuilt'"
            BuildTimeout -> "'BuildTimeout'"

        migrateUserInfo (uId, _u) = do
            -- Get user_information.
            (Just (Entity _ ui)) <- runDB $ selectFirst [UserInformationUser ==. uId] []
            let user = toSQLVariable uId
            let school = sqlMaybe sqlText $ userInformationSchool ui
            let major = sqlMaybe sqlText $ userInformationMajor ui
            let minor = sqlMaybe sqlText $ userInformationMinor ui
            let degreesHeld = sqlMaybe sqlText $ userInformationDegreesHeld ui
            let degree = sqlText $ userInformationDegree ui
            let yearsInProgram = sqlMaybe sqlInt $ userInformationYearsInProgram ui
            let yearsOfExperience = sqlMaybe sqlInt $ userInformationYearsOfExperience ui
            let languages = sqlMaybe sqlText $ userInformationLanguages ui
            let favoriteLanguages = sqlMaybe sqlText $ userInformationFavoriteLanguages ui
            let yearsOfWork = sqlMaybe sqlInt $ userInformationYearsOfWork ui
            let experienceClass = sqlMaybe sqlBool $ userInformationExperienceClass ui
            let experiencePersonal = sqlMaybe sqlBool $ userInformationExperiencePersonal ui
            let experienceInternship = sqlMaybe sqlBool $ userInformationExperienceInternship ui
            let experienceJob = sqlMaybe sqlBool $ userInformationExperienceJob ui
            let securityTraining = sqlMaybe sqlBool $ userInformationSecurityTraining ui
            let securityExperience = sqlMaybe sqlBool $ userInformationSecurityExperience ui
            let softwareEngineering = sqlMaybe sqlBool $ userInformationSoftwareEngineering ui
            let securityClass = sqlMaybe sqlBool $ userInformationSecurityClass ui
            let previousContest = userInformationPreviousContest ui
            let resumePermission = sqlBool $ userInformationResumePermission ui
            let age = sqlMaybe sqlInt $ userInformationAge ui
            let nationality = sqlMaybe sqlText $ userInformationNationality ui
            let gender = sqlMaybe sqlText $ userInformationGender ui
            let agreeToParticipate = sqlBool $ userInformationAgreeToParticipate ui
            let graduationYear = sqlMaybe sqlInt $ userInformationGraduationYear ui
            let programmerRating = sqlMaybe sqlInt $ userInformationProgrammerRating ui
            let attackerRating = sqlMaybe sqlInt $ userInformationAttackerRating ui
            let language = sqlMaybe sqlText $ userInformationLanguage ui
            let timezone = sqlMaybe sqlText $ userInformationTimezone ui
            let vals = error "TODO: Update this..." -- List.intercalate ", " [ user, school, major, minor, degreesHeld, degree, yearsInProgram, yearsOfExperience, languages, favoriteLanguages, yearsOfWork, experienceClass, experiencePersonal, experienceInternship, experienceJob, securityTraining, securityExperience, softwareEngineering, securityClass, previousContest, resumePermission, age, nationality, gender, agreeToParticipate, graduationYear, programmerRating, attackerRating, language, timezone]
            printSQL $ "INSERT INTO user_information (\"user\", school, major, minor, degrees_held, degree, years_in_program, years_of_experience, languages, favorite_languages, years_of_work, experience_class, experience_personal, experience_internship, experience_job, security_training, security_experience, software_engineering, security_class, previous_contest, resume_permission, age, nationality, gender, agree_to_participate, graduation_year, programmer_rating, attacker_rating, language, timezone) VALUES ( " ++ vals ++ ") "

        migrateTeamMember tId (uId, _) = 
            printSQL $ "INSERT INTO team_member (\"user\", team) VALUES ( " ++ toSQLVariable uId ++ ", " ++ toSQLVariable tId ++ ")"

        migrateTeam (tId, t, leaderId) = 
            let name = Text.unpack $ teamName t in
            let leader = toSQLVariable leaderId in
            printSQL $ "INSERT INTO team (name, leader) VALUES ( '" ++ name ++ "', "++ leader ++") RETURNING id INTO " ++ toSQLVariable tId

        migrateTeamContest (tcId, tc, tId) =
            let team = toSQLVariable tId in
            let contest = targetContest in
            let git = Text.unpack $ teamContestGitUrl tc in
            let languages = Text.unpack $ teamContestLanguages tc in
            printSQL $ "INSERT INTO team_contest (team, contest, git_url, languages) VALUES (" ++ team ++ ", " ++ contest ++ ", '" ++ git ++ "', '" ++ languages ++ "') RETURNING id INTO " ++ toSQLVariable tcId

        
        migrateBuildSubmission tcId ((Entity bsId bs), cores, perfs, opts) = do
            let team = toSQLVariable tcId
            let time = sqlTime $ buildSubmissionTimestamp bs
            let hash = sqlText $ buildSubmissionCommitHash bs
            let status = sqlStatus $ buildSubmissionStatus bs
            let vals = List.intercalate ", " [team, time, hash, status]
            printSQL $ "INSERT INTO build_submission (team, \"timestamp\", commit_hash, status) VALUES (" ++ vals ++ ") RETURNING id INTO " ++ toSQLVariable bsId

            mapM_ (migrateCoreResult bsId) cores
            mapM_ (migratePerformanceResult bsId) perfs
            -- mapM_ (migrateOptionalResult bsId) opts
        
        migrateCoreResult bsId (Entity _ core) = 
            let submission = toSQLVariable bsId in
            let test = maybe (error "migrateCoreResult: invalid test id") id $ List.lookup (keyToInt $ buildCoreResultTest core) coreTestMapping in
            let pass = sqlBool $ buildCoreResultPass core in
            let vals = List.intercalate ", " [ submission, test, pass] in
            printSQL $ "INSERT INTO build_core_result ( submission, test, pass) VALUES (" ++ vals ++ ")"
        migratePerformanceResult bsId (Entity _ perf) = 
            let submission = toSQLVariable bsId in
            let test = maybe (error "migratePerformanceResult: invalid test id") id $ List.lookup (keyToInt $ buildPerformanceResultTest perf) performanceTestMapping in
            let time = sqlMaybe sqlDouble $ buildPerformanceResultTime perf in
            let vals = List.intercalate ", " [ submission, test, time] in
            printSQL $ "INSERT INTO build_performance_result ( submission, test, time) VALUES (" ++ vals ++ ")"

        -- Note: don't need for this contest.
        -- migrateOptionalResult = undefined

        migrateBuildScore tcId (Entity _ score) = 
            let team = toSQLVariable tcId in
            let buildScore = sqlMaybe sqlDouble $ teamBuildScoreBuildScore score in
            let breakScore = sqlMaybe sqlDouble $ teamBuildScoreBreakScore score in
            let fixScore = sqlMaybe sqlDouble $ teamBuildScoreFixScore score in
            let timestamp = sqlTime $ teamBuildScoreTimestamp score in
            let vals = List.intercalate ", " [ team, buildScore, breakScore, fixScore, timestamp] in
            printSQL $ "INSERT INTO team_build_score (team, build_score, break_score, fix_score, \"timestamp\") VALUES (" ++ vals ++ ")"

        migrateBreakScore tcId (Entity _ score) = 
            let team = toSQLVariable tcId in
            let buildScore = sqlMaybe sqlDouble $ teamBreakScoreBuildScore score in
            let breakScore = sqlMaybe sqlDouble $ teamBreakScoreBreakScore score in
            let fixScore = sqlMaybe sqlDouble $ teamBreakScoreFixScore score in
            let timestamp = sqlTime $ teamBreakScoreTimestamp score in
            let vals = List.intercalate ", " [ team, buildScore, breakScore, fixScore, timestamp] in
            printSQL $ "INSERT INTO team_break_score (team, build_score, break_score, fix_score, \"timestamp\") VALUES (" ++ vals ++ ")"
        


class ToSQLVariable a where
    toSQLVariable :: a -> String

instance ToSQLVariable UserId where
    toSQLVariable i = "userid" ++ show (keyToInt i)

instance ToSQLVariable TeamId where
    toSQLVariable i = "teamid" ++ show (keyToInt i)

instance ToSQLVariable TeamContestId where
    toSQLVariable i = "teamcontestid" ++ show (keyToInt i)

instance ToSQLVariable BuildSubmissionId where
    toSQLVariable i = "buildsubmissionid" ++ show (keyToInt i)

