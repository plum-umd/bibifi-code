module Handler.Participation.Information (getParticipationInformationR, postParticipationInformationR, postParticipationInformationUnregisterR) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding
import Forms
import Import
import Network.Mail.Mime
import qualified Participation
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Shakespeare.Text (stext)

form :: TeamContest -> Form TeamContest
form team = 
    let tId = teamContestTeam team in
    let cId = teamContestContest team in
    let url = teamContestGitUrl team in
    let professional = teamContestProfessional team in
    let language = teamContestLanguages team in
    let gitSettings = bootstrapify $ addAttribute "Git URL" "placeholder" "git@domain.com:user/repo.git" in 
    let languageSettings = bootstrapify $ addAttribute "Languages" "placeholder" "Best guess if currently unsure" in
    renderBootstrap3 layout $ TeamContest
        <$> pure tId
        <*> pure cId
        <*> areq gitField gitSettings (Just url) -- TODO: change type to some sort of git url parsing?? urlField??
        <*> areq programmingLanguageField languageSettings (Just language) -- TODO: change to special language field?
        <*> pure professional

    where
        gitField = check (\t -> 
                if Text.take 15 t == "git@gitlab.com:" || Text.take 18 t == "git@bitbucket.org:" || Text.take 15 t == "git@github.com:" then
                    Right t
                else
                    Left ("Must be a git ssh url from gitlab, bitbucket, or github (ie, git@domain.com:user/repo.git)." :: Text)
            ) textField

        layout = BootstrapHorizontalForm (ColXs 0) (ColXs 2) (ColXs 0) (ColXs 10)

generateHtml :: Contest -> Team -> TeamContestId -> TeamContest -> UserId -> Widget -> Enctype -> Widget -> Enctype -> [Text] -> LWidget
generateHtml contest team tcId tc uId widget enctype widgetU enctypeU msg = do
    let msgH = mconcat $ map displayError msg
    unregisterW <- do
            -- Check if contest has started.
            errorM <- handlerToWidget $ unregisteringIsAllowed tc team contest uId
            return $ case errorM of
                Just err ->
                    [whamlet'|
                        <h3>
                            Unregister from contest
                        <p>
                            #{err}
                    |]
                Nothing ->
                    [whamlet'|
                        <h3>
                            Unregister from contest
                        <form method=post action="@{ParticipationInformationUnregisterR tcId}" enctype=#{enctypeU}>
                            ^{widgetU}
                            <p .text-danger>
                                Warning: Make sure all of your team members agree to unregistering from the contest. 
                            <button type="submit" class="btn btn-danger">
                                Unregister
                    |]
    [whamlet|
        ^{msgH}
        <form method=post action=@{ParticipationInformationR tcId} enctype=#{enctype} class="form-horizontal" role="form">
            <div class="form-group">
                <label class="col-sm-2 control-label">
                    Contest
                <div class="col-sm-10">
                    <p class="form-control-static">
                        #{contestTitle contest}
            <div class="form-group">
                <label class="col-sm-2 control-label">
                    Team
                <div class="col-sm-10">
                    <p class="form-control-static">
                        #{teamName team}
            ^{widget}
            <div class="form-group">
                <div class="col-sm-offset-2 col-sm-10">
                    <button type="submit" class="btn btn-default">
                        Submit
        ^{unregisterW}
    |]

getParticipationInformationR :: TeamContestId -> Handler Html
getParticipationInformationR tcId = runLHandler $ 
    Participation.layout Participation.Information tcId $ \uId tc contest team -> do
        -- Check if team leader
        if (teamLeader team) == uId then
            do
            ( widget, enctype) <- handlerToWidget $ generateFormPost $ form tc
            ( widgetU, enctypeU) <- handlerToWidget $ generateFormPost unregisterForm
            generateHtml contest team tcId tc uId widget enctype widgetU enctypeU []
        else
            -- Just display information.
                [whamlet|
                    <form class="form-horizontal">
                        <div class="form-group">
                            <label class="col-sm-2 control-label">
                                Team
                            <div class="col-sm-10">
                                <p class="form-control-static">
                                    #{teamName team}
                        <div class="form-group">
                            <label class="col-sm-2 control-label">
                                Contest
                            <div class="col-sm-10">
                                <p class="form-control-static">
                                    #{contestTitle contest}
                        <div class="form-group">
                            <label class="col-sm-2 control-label">
                                Git URL
                            <div class="col-sm-10">
                                <p class="form-control-static">
                                    #{teamContestGitUrl tc}
                    <p class="col-sm-offset-2">
                        Note: Only the team leader can change this information.
                |]

postParticipationInformationR :: TeamContestId -> Handler Html
postParticipationInformationR tcId = runLHandler $
    Participation.layout Participation.Information tcId $ \uId oldTc contest team -> do
        -- Check if team leader
        if (teamLeader team) == uId then
            do
            ((res, widget), enctype) <- handlerToWidget $ runFormPost $ form oldTc
            ( widgetU, enctypeU) <- handlerToWidget $ generateFormPost unregisterForm
            case res of
                FormSuccess teamcontest -> do
                    handlerToWidget $ runDB $ updateWhere [ TeamContestContest ==. (teamContestContest teamcontest), TeamContestTeam ==. (teamContestTeam teamcontest)] [TeamContestGitUrl =. (teamContestGitUrl teamcontest), TeamContestLanguages =. (teamContestLanguages teamcontest)]
                    
                    -- E.updateCount $ \tc -> do
                    --     E.set tc [ TeamContestGitUrl E.=. E.val (teamContestGitUrl teamcontest)]
                    --     E.set tc [ TeamContestLanguages E.=. E.val (teamContestLanguages teamcontest)]
                    --     E.where_ ( tc E.^. TeamContestContest E.==. E.val (teamContestContest teamcontest)
                    --         E.&&. tc E.^. TeamContestTeam E.==. E.val (teamContestTeam teamcontest))
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Successfully updated team participation information.
                    |]
                    redirect $ ParticipationInformationR tcId
                        
                FormFailure _msg ->
                    generateHtml contest team tcId oldTc uId widget enctype widgetU enctypeU []
                FormMissing ->
                    generateHtml contest team tcId oldTc uId widget enctype widgetU enctypeU []
        else
            redirect $ ParticipationInformationR tcId

postParticipationInformationUnregisterR :: TeamContestId -> Handler Html
postParticipationInformationUnregisterR tcId = runLHandler $ Participation.layout Participation.Information tcId $ \uId tc contest team -> do
    errM <- handlerToWidget $ unregisteringIsAllowed tc team contest uId
    case errM of
        Just _ ->
            errHandler errM
        Nothing -> do
            ((res, widget), enctype) <- handlerToWidget $ runFormPost $ unregisterForm
            case res of
                FormFailure _msg ->
                    errHandler Nothing
                FormMissing ->
                    errHandler Nothing
                FormSuccess _ -> do
                    -- Email all team members.
                    emailMembers (teamContestTeam tc) team uId contest

                    -- Delete team contest.
                    handlerToWidget $ runDB $ delete tcId

                    -- Set message.
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Successfully unregistered team from contest.
                    |]

                    -- Redirect.
                    redirect $ SpecificAnnouncementsR $ contestUrl contest

    where
        errHandler errM = do
            let err = maybe "Error, could not unregister team." id errM
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        #{err}
            |]
            redirect $ ParticipationInformationR tcId
        
        emailMembers tId team uId contest = do
            -- Get username.
            username <- do
                userM <- handlerToWidget $ runDB $ get uId
                return $ maybe "UNKNOWN" userIdent userM

            -- Get their email addresses.
            to <- handlerToWidget $ do
                members <- runDB [lsql| select User.email from User inner join TeamMember on TeamMember.user == User.id where TeamMember.team == #{tId} |]
                leaderM <- runDB $ get $ teamLeader team
                let emails = case leaderM of
                      Nothing ->
                        members
                      Just leader ->
                        (userEmail leader):members
                return $ map (Address Nothing) emails
            let head = [("Subject", "Team unregistered for BIBIFI contest")]
            let text = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Hi team #{teamName team},

Just to let you know, your team leader, #{username}, has unregistered your team from the Build-it Break-it contest. If you need help finding a new team, email us at info@builditbreakit.org. 

Best,

BIBIFI organizers
|]
            let textPart = Part {
                partType = "text/plain; charset=utf-8",
                partEncoding = None,
                partFilename = Nothing,
                partContent = text,
                partHeaders = []
            }
            let html = renderHtml [shamlet|
                <p>
                    Hi team #{teamName team},
                <p>
                    Just to let you know, your team leader, #{username}, has unregistered your team from the Build-it Break-it contest. If you need help finding a new team, email us at info@builditbreakit.org. 
                <p>
                    Best,
                <p>
                    BIBIFI organizers
            |]
            let htmlPart = Part { 
                partType = "text/html; charset=utf-8",
                partEncoding = None,
                partFilename = Nothing,
                partContent = html,
                partHeaders = []
            }
            lLift $ liftIO $ renderSendMail (emptyMail $ Address (Just "Build it Break it Fix it") "noreply@builditbreakit.org")
                {mailTo = to, mailHeaders = head, mailParts = [[textPart, htmlPart]]}

                

unregisteringIsAllowed :: TeamContest -> Team -> Contest -> UserId -> LHandler (Maybe String)
unregisteringIsAllowed tc team contest uId = 
    -- Check if team leader.
    if (teamLeader team) /= uId then
        return $ Just "Only team leaders can unregister their team from the contest."
    else do
        -- Check if contest has started.
        now <- getCurrentTime
        if now >= contestBuildStart contest then
            return $ Just "Cannot unregister. The contest has already started."
        else
            return Nothing

data UnregisterForm = UnregisterForm ()

unregisterForm = renderBootstrap3 BootstrapBasicForm $ UnregisterForm
    <$> pure ()


