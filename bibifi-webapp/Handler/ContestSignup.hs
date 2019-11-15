module Handler.ContestSignup (getContestSignupR, getContestSpecificSignupR, postContestSpecificSignupR) where

import Import
import Control.Monad (foldM)
import qualified Data.Text as T
import Yesod.Auth.OAuth2.Coursera

import Coursera

data SelectTeam = SelectTeam TeamId

registerForm :: [(Text,TeamId)] -> Form SelectTeam
registerForm teamList = renderDivs $ SelectTeam
    <$> areq (selectFieldList teamList) "Select team" Nothing

unlessDeadline :: Contest -> LWidget -> LWidget
unlessDeadline c f = do
    now <- getCurrentTime
    if now >= (contestBreakEnd c) then
        [whamlet|
            Sorry, registration for this contest is closed.
        |]
    else
        f

showRegistration :: Widget -> Enctype -> [Text] -> UserId -> Entity Contest -> LWidget
showRegistration widget enctype msg userId c =
    let Entity contestId contest = c in
    let msgH = mconcat $ map displayError msg in
    let url = contestUrl contest in
    do
    -- Check if already signed up.
    signedup <- handlerToWidget $ userIsSignedupForContest userId contestId
    if signedup then
        [whamlet|
            You are already signed up for this contest! View your information <a href="@{ContestParticipationR url}">here</a>.
        |]
    else
        -- Check if contest deadline passed.
        unlessDeadline contest $ do
            [whamlet|$newline never
                ^{msgH}
                <form method=post action=@{ContestSpecificSignupR url} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit value="Sign Up">
                <div>
                    If you are not the team leader, please ask your team leader to sign up, or <a href=@{CreateTeamR}>create your own team</a>.
            |]

generateHtml :: Text
    -> ([(Text,TeamId)] -> LHandler (UserId -> Entity Contest -> LWidget))
    -> LHandler Html
generateHtml url f = do 
    -- TODO: Add a deadline for registration? Sorry, the contest has already started. Registration is closed.
    -- Force log in.
    uId <- requireAuthId
    raiseGroupLabel
    raiseTeamLabel
    -- Get contest information.
    contest <- retrieveContest $ Just url
    let title = generatePageTitle contest "Contest Sign Up"
    -- Retrieve teams that user is leader of.
    teams <- runDB $ selectList [TeamLeader ==. uId] []
    let content contest@(Entity contestId c) = do
        -- Check if contest is a coursera one. 
        cStatus <- handlerToWidget $ checkCourseraContest contestId uId 
        if cStatus /= CourseraStatusEnrolled && cStatus /= CourseraStatusNotCoursera then do
            setUltDestCurrent
            [whamlet|
                You must be enrolled in the Coursera capstone to participate in this run of the contest. If you are enrolled, please <a href="@{AuthR oauth2CourseraUrl}">log in</a> via coursera. 
            |]
        else do
            -- Check if teams is empty.
            case teams of
                [] -> do
                    -- Check if already signed up.
                    signedup <- handlerToWidget $ userIsSignedupForContest uId contestId
                    if signedup then
                        [whamlet|
                            You are already signed up for this contest! View your information <a href="@{ContestParticipationR url}">here</a>.
                        |]
                    else
                        -- Check if contest deadline passed.
                        unlessDeadline c $ 
                            [whamlet|
                                You are not the leader of any teams. Please ask your team leader to sign up, or <a href=@{CreateTeamR}>create your own team</a>. 
                            |]
                ts -> do
                    let mapping (Entity tId t) = (teamName t, tId)
                    let teamList = map mapping ts
                    g <- handlerToWidget $ f teamList
                    g uId contest
    customLayout contest $ do
        setTitle $ toHtml title
        contestTemplate contest "Contest Sign Up" content


-- generateHtmlHelper :: Widget -> Enctype -> [Text] -> Text -> HandlerHtml
-- generateHtmlHelper widget enctype msg contestId = 
--     -- Get contest information.
--     contest <- retrieveContest $ Just contestId
--     title <- generatePageTitle contest "Contest Registration"
--     -- Retrieve teams that user is leader of.
--     teams <- runDB $ selectList [TeamLeader ==. uId] []
--     -- Check if teams is empty.
--     content <- case teams of
--         [] -> 
--         ts ->

-- Get handler.
getContestSpecificSignupR :: Text -> Handler Html
getContestSpecificSignupR contestId = runLHandler $
    generateHtml contestId (\teamList -> do
        ( widget, enctype) <- generateFormPost $ registerForm teamList
        return $ showRegistration widget enctype []
    )

-- Post handler.
postContestSpecificSignupR :: Text -> Handler Html
postContestSpecificSignupR url = runLHandler $ 
    generateHtml url (\teamList -> do
        -- TODO: implement this
        -- check if leader is already on a team for this contest (do earlier??)
        --  do the same for all signed up teammates
        ((res, widget), enctype) <- runFormPost $ registerForm teamList
        -- errorRegistration l <- return $ (\_ -> showRegistration widget enctype l)
        case res of 
            FormFailure _msg ->
                return $ showRegistration widget enctype [] -- msg
            FormMissing ->
                return $ showRegistration widget enctype []
            FormSuccess (SelectTeam teamId) ->
                return (\userId contest -> do
                    (Entity contestId c) <- return contest
                    -- Check if contest deadline has already passed. 
                    unlessDeadline c $ do
                        -- Check if leader can join the contest.
                        res' <- handlerToWidget $ runDB $ get teamId
                        case res' of 
                            Nothing ->
                                showRegistration widget enctype ["Your team doesn't exist..."] userId contest
                            Just team -> do
                                leaderJoin <- handlerToWidget $ userCanJoinTeamForContest (teamLeader team) teamId contestId
                                case leaderJoin of
                                    Right () -> do
                                        -- Check if team members can join contest.
                                        members <- handlerToWidget $ runDB $ selectList [TeamMemberTeam ==. teamId] []
                                        memberJoin <- foldM (\acc member'@(Entity _ member) -> case acc of
                                            Right () -> do
                                                let memberId = teamMemberUser member
                                                handlerToWidget $ userCanJoinTeamForContest memberId teamId contestId
                                            Left _ ->
                                                return acc
                                          ) (Right ()) members
                                        case memberJoin of
                                            Left err -> 
                                                showRegistration widget enctype [err] userId contest
                                            Right () -> do
                                                res'' <- handlerToWidget $ runDB $ insertUnique $ TeamContest teamId contestId "" "" False undefined -- TODO: get git url...
                                                case res'' of
                                                    Nothing ->
                                                        showRegistration widget enctype ["Your team is already signed up for this contest."] userId contest
                                                    Just tcId -> do
                                                        handlerToWidget $ setMessage [shamlet|
                                                            <div class="container">
                                                                <div class="alert alert-success">
                                                                    Successfully registered for contest!
                                                        |]
                                                        handlerToWidget $ redirect $ ParticipationInformationR tcId
                                    Left err ->
                                        showRegistration widget enctype [err] userId contest
                                        -- showRegistration widget enctype ["You already are participating in this contest."] userId contest
                ) 
    ) 

getContestSignupR :: Handler Html
getContestSignupR = runLHandler $ do
    res <- getConfig DefaultContest
    contest <- retrieveContest res
    case contest of 
        -- Redirect if there is a current contest.
        Just (Entity _ c) ->
            redirect $ ContestSpecificSignupR $ contestUrl c
        -- Otherwise, give error that no contest was found. 
        Nothing -> do
            let title = generatePageTitle contest "Contest Sign Up"
            customLayout contest $ do 
                setTitle $ toHtml title
                contestTemplate contest "Contest Sign Up" (\_ -> return ())
    
