module Contest ( userCanJoinTeamForContest, userIsSignedupForContest, retrieveContest, generateContestTitle, generatePageTitle, contestTemplate, isDefaultContest) where

import Prelude
import LMonad.Yesod
import LYesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.LPersist
import Model
import Config
import Coursera
-- import Foundation
import Control.Monad (foldM)

-- Check that none of the teams are registered for the contest. 
-- !signup(t1) and !signup(t2) and ...
-- !(signup(t1) or signup(t2) or ...)
checkTeams :: ContestId -> [Entity Team] -> LHandler Bool
checkTeams contestId = foldM (\acc team' -> do 
        if acc then do
            (Entity teamId' _) <- return team'
            res <- runDB $ getBy $ UniqueTeamContest teamId' contestId
            return $ case res of 
                -- Not signed up.
                Nothing -> 
                    True
                -- Signed up.
                Just _ ->
                    False
        else
            return False
    ) True

-- Checks if a user can join a team. 
-- This is true as long as all of the user's other teams are not registered for the current contest.
-- For coursera contests, the user must also be enrolled in the course. 
userCanJoinTeamForContest :: UserId -> TeamId -> ContestId -> LHandler (Either Text ())
userCanJoinTeamForContest userId teamId contestId = do
    leaderTeams <- runDB $ selectList [TeamLeader ==. userId, TeamId !=. teamId] []
    res <- checkTeams contestId leaderTeams
    -- None of the leader teams are signed up.
    if res then do
        memberTeams <- runDB [lsql|SELECT Team.* FROM Team INNER JOIN TeamMember ON TeamMember.team == Team.id WHERE TeamMember.user == #{userId} and TeamMember.team != #{teamId}|]
        -- memberTeams <- runDB $ do
        --     res <- E.select $ E.from $ \(t `E.InnerJoin` tm) -> do
        --         E.on ( t E.^. TeamId E.==. tm E.^. TeamMemberTeam)
        --         E.where_ (tm E.^. TeamMemberUser E.==. E.val userId E.&&. tm E.^. TeamMemberTeam E.!=. E.val teamId)
        --         return t
        --     mapM (\(v) -> return (v :: Entity Team)) res
            
        -- select * from TeamMember, Team where TeamMember.User == userId && TeamMember.Team != teamId && TeamMember.Team == Team.Id
        -- E.select $ E.from $ \( m, t) -> do
        --     E.where_ ( m E.^. TeamMemberUser E.==. E.val userId 
        --         E.&&. m E.^. TeamMemberTeam E.!=. E.val teamId
        --         E.&&. m E.^. TeamMemberTeam E.==. t E.^. TeamId)
        --     return t
        notOnTeam <- checkTeams contestId memberTeams
        if notOnTeam then do
            -- Check if this is a Coursera contest. 
            status <- checkCourseraContest contestId userId
            case status of
                CourseraStatusEnrolled -> do
                    -- Check that team limit isn't reached. 
                    leaderCount <- runDB $ count [TeamId ==. teamId, TeamLeader !=. userId]
                    memberCount <- runDB $ count [TeamMemberTeam ==. teamId, TeamMemberUser !=. userId]
                    if leaderCount + memberCount >= teamLimit then
                        return $ Left "Team already has the maximum number of team members."
                    else
                        return $ Right ()
                CourseraStatusNotCoursera ->
                    return $ Right ()
                CourseraStatusNotEnrolled ->
                    errorWithUserName " is not enrolled in the Coursera course. Please ensure the user is logged in via Coursera and enrolled in the capstone course."
                CourseraStatusError ->
                    return $ Left "Could not verify Coursera enrollment status at this time. Please try to clear your cookies, try a different browser, and/or log in/out of coursera.org."
        else
            errorWithUserName " is already on a team that is signed up."

    -- At least one of the leader teams is signed up.
    else
        errorWithUserName " is already the leader of a team that is signed up."

    where
        teamLimit = 5

        errorWithUserName err = do
            userM <- runDB $ get userId
            return $ Left $ maybe ("User not found.") (\user -> 
                userIdent user `T.append` err) userM

-- Check if a user is signed up for a contest.
userIsSignedupForContest :: UserId -> ContestId -> LHandler Bool
userIsSignedupForContest userId contestId = do
    leaderTeams <- runDB $ selectList [TeamLeader ==. userId] []
    res <- checkTeams contestId leaderTeams
    -- None of the leader teams are signed up.
    if res then do
        memberTeams <- runDB [lsql| select Team.* from TeamMember inner join Team on TeamMember.team == Team.id where TeamMember.user == #{userId}|]
        
        
        -- $ E.select $ E.from $ \( m, t) -> do
        --     -- select * from TeamMember, Team where TeamMember.User == userId && TeamMember.Team == Team.Id
        --     E.where_ ( m E.^. TeamMemberUser E.==. E.val userId 
        --         E.&&. m E.^. TeamMemberTeam E.==. t E.^. TeamId)
        --     return t
        res' <- checkTeams contestId memberTeams
        return $ not res'
    -- At least one of the leader teams is signed up for the contest. 
    else
        return True

defaultContest :: LHandler (Maybe (Entity Contest))
defaultContest = do
    defaultContest <- getConfig DefaultContest
    case defaultContest of
        Nothing ->
            return Nothing
        Just c ->
            runDB $ getBy $ UniqueContest c

retrieveContest :: Maybe Text -> LHandler (Maybe (Entity Contest))
retrieveContest contestId = case contestId of
    Nothing ->
        defaultContest
    Just c ->
        runDB $ getBy $ UniqueContest c

generateContestTitle :: Maybe (Entity Contest) -> Text
generateContestTitle contest =
    case contest of
        Nothing ->
            "BIBIFI Contest"
        Just (Entity _ c) ->
            contestTitle c

generatePageTitle :: Maybe (Entity Contest) -> Text -> Text
generatePageTitle contest page =
    let t = generateContestTitle contest in
    T.concat [ page, " - ", t]
    -- return $ case contest of
    --     Nothing ->
    --         page
    --     Just (Entity _ c) ->
    --         T.concat [ page, " - ", contestTitle c]

isDefaultContest :: LHandler (Entity Contest -> Bool)
isDefaultContest = do
    res <- getConfig DefaultContest
    case res of 
        Nothing ->
            return $ \_ -> False
        Just def ->
            return $ \(Entity _ c) -> def == (contestUrl c)

contestTemplate :: Maybe (Entity Contest) -> Text -> (Entity Contest -> LWidget) -> LWidget
contestTemplate contest page content = 
    let content' = case contest of 
          Nothing ->
              [whamlet|$newline never
<div class="row">
    <div class="col-md-12">
        Sorry, contest was not found.
|]
          Just c ->
              content c
    in
    let contestName = generateContestTitle contest in
    do
    content'' <- extractWidget content'
    [whamlet|$newline never
        <div class="row">
            <div class="col-md-12">
                <div class="page-header">
                    <h1>
                        #{page} 
                        <small>
                            #{contestName}
        ^{content''}
    |]

