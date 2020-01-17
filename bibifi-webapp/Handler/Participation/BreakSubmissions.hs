module Handler.Participation.BreakSubmissions where

import qualified Database.Esqueleto as E
import Score

import qualified BreakSubmissions
import Import
import qualified Participation
import PostDependencyType
import Submissions

getParticipationBreakSubmissionsR :: TeamContestId -> Handler Html
getParticipationBreakSubmissionsR tcId = runLHandler $ 
    Participation.layout Participation.BreakSubmissions tcId $ \_ _ contest _ -> do
        -- submissions <- handlerToWidget $ runDB $ selectList [BreakSubmissionTeam ==. tcId] [Desc BreakSubmissionTimestamp]
        submissions <- handlerToWidget $ runDB $ [lsql| select BreakSubmission.*, Team.name from BreakSubmission inner join TeamContest on BreakSubmission.targetTeam == TeamContest.id inner join Team on TeamContest.team == Team.id where BreakSubmission.team == #{tcId} order by BreakSubmission.id desc |]
        -- E.select $ E.from $ \( s `E.InnerJoin` tc `E.InnerJoin` tt) -> do
        --     E.on ( tc E.^. TeamContestTeam E.==. tt E.^. TeamId)
        --     E.on ( s E.^. BreakSubmissionTargetTeam E.==. tc E.^. TeamContestId)
        --     E.where_ ( s E.^. BreakSubmissionTeam E.==. E.val tcId)
        --     E.orderBy [E.desc (s E.^. BreakSubmissionTimestamp)]
        --     return (s, tt E.^. TeamName)
        case submissions of
            [] ->
                [whamlet|
                    <p>
                        No submissions were found. If you have made submissions, please ensure your git url is correct on the information page.
                |]
            _ ->
                displayBreakSubmissionsTable contest BreakSubmissionAttacker submissions

        let againstW = do
            ss <- handlerToWidget $ runDB $ [lsql| select BreakSubmission.*, Team.name from BreakSubmission inner join TeamContest on BreakSubmission.team == TeamContest.id inner join Team on Team.id == TeamContest.team where BreakSubmission.targetTeam == #{Just tcId} order by BreakSubmission.id desc |]
            -- E.select $ E.from $ \( s `E.InnerJoin` tc `E.InnerJoin` tt) -> do
            --     E.on ( tc E.^. TeamContestTeam E.==. tt E.^. TeamId)
            --     E.on ( s E.^. BreakSubmissionTeam E.==. tc E.^. TeamContestId)
            --     E.where_ ( s E.^. BreakSubmissionTargetTeam E.==. E.val tcId)
            --     E.orderBy [E.desc (s E.^. BreakSubmissionTimestamp)]
            --     return (s, tt E.^. TeamName)
            case ss of 
                [] ->
                    [whamlet|
                        <p>
                            No submissions have targeted against your team.
                    |]
                _ ->
                    displayBreakSubmissionsTable contest BreakSubmissionVictim ss
        [whamlet|
            <h3>
                Against your team
        |]
        againstW

getParticipationBreakSubmissionR :: TeamContestId -> BreakSubmissionId -> Handler Html
getParticipationBreakSubmissionR tcId bsId = runLHandler $ do
    bs <- BreakSubmissions.checkBreakSubmissionTeam tcId bsId
    let victim = breakSubmissionTargetTeam bs == Just tcId
    Participation.layout Participation.BreakSubmissions tcId $ \userId teamcontest contest team -> do
        let status = (if victim then
                prettyBreakStatusVictim
              else
                prettyBreakStatus
              ) $ breakSubmissionStatus bs
        let result = (if victim then
                prettyBreakValidVictim
              else
                prettyBreakValid
              ) $ breakSubmissionValid bs
        let message = case breakSubmissionMessage bs of
              Nothing -> dash
              Just msg -> [shamlet|
                <span>
                    #{msg}
                |]
        let testType = case breakSubmissionBreakType bs of
              Nothing -> dash
              Just typ -> prettyBreakType typ 
        time <- displayTime $ breakSubmissionTimestamp bs
        (attackTeamName,targetTeam) <- do
            res <- handlerToWidget $ runDB $ BreakSubmissions.getBothTeams bsId $ \(Entity _ t) _tc _bs _tct (Entity _ tt) ->
                ( teamName t, tt)
            case res of
                Just names ->
                    return names
                _ ->
                    notFound

        let targetTeamName = teamName targetTeam

        -- Judgement widget.
        judgementW <- do
            judgementM <- handlerToWidget $ runDB $ getBy $ UniqueBreakJudgement bsId
            return $ case judgementM of
                Nothing ->
                    mempty
                Just (Entity jId j) ->
                    let ruling = case breakJudgementRuling j of
                          Nothing ->
                            [shamlet|
                                <span>
                                    Pending
                            |]
                          Just True ->
                            [shamlet|
                                <span class="text-success">
                                    Passed
                            |]
                          Just False ->
                            [shamlet|
                                <span class="text-danger">
                                    Failed
                            |]
                    in
                    let comments = case breakJudgementComments j of
                          Nothing ->
                            dash
                          Just c ->
                            toHtml c
                    in
                    [whamlet'|
                        <div class="form-group">
                            <label class="col-xs-3 control-label">
                                Judgment
                            <div class="col-xs-9">
                                <p class="form-control-static">
                                    #{ruling}
                        <div class="form-group">
                            <label class="col-xs-3 control-label">
                                Judge comments
                            <div class="col-xs-9">
                                <p class="form-control-static">
                                    #{comments}
                    |]

        now <- getCurrentTime

        disputeW <- do
            -- TODO: Check if a dispute exists. 
            disputeM <- handlerToWidget $ runDB $ getBy $ UniqueBreakDispute bsId
            case disputeM of
                Just (Entity _ (BreakDispute _ justification)) ->
                    return [whamlet'|
                        <h3>
                            Dispute
                        <form class="form-horizontal">
                            <div class="form-group">
                                <label class="col-xs-3 control-label">
                                    Justification
                                <div class="col-xs-9">
                                    <p class="form-control-static">
                                        #{justification}
                    |]
                Nothing -> 
                    if not development && (not victim || now > contestBreakEnd contest) then
                      return mempty
                    else
                      -- Check if team leader.
                      if userId /= teamLeader targetTeam then
                          return [whamlet'|
                              <h3>
                                  Dispute
                              <p>
                                  Only the team leader may dispute a break submission. 
                          |]
                      else
                          -- Check that the break result is accepted.
                          -- let res = breakSubmissionResult bs in
                          -- if res /= Just BreakCorrect && res /= Just BreakExploit then
                          if breakSubmissionValid bs /= Just True then
                              return [whamlet'|
                                  <h3>
                                      Dispute
                                  <p>
                                      You may only dispute accepted breaks.
                              |]
                          else do
                              --TODO: Check if already disputed
                              --TODO: Email organizers...
                              (disputeW, disputeE) <- handlerToWidget $ generateFormPost disputeBreakSubmissionForm
                              return [whamlet'|
                                  <h3>
                                      Dispute
                                  <p>
                                      If you believe this break is incorrect, write a justification for why you think it is invalid. The justification (no more than 100 words) should indicate why your system's behavior is acceptable according to the spec, even if the oracle behaves differently. 
                                      We will review the dispute during the judging phase. 
                                  <form role=form method=post action="@{ParticipationBreakSubmissionDisputeR tcId bsId}" enctype=#{disputeE}>
                                      ^{disputeW}
                              |]
                        

        -- Build output widget.
        let buildOutputW = case (victim, breakSubmissionStatus bs, breakSubmissionStdout bs, breakSubmissionStderr bs) of
              (False, BreakRejected, Just stdout, Just stderr) ->
                [whamlet'|
                    <h4>
                        Standard Output from Make
                    <samp>
                        #{stdout}
                    <h4>
                        Standard Error from Make
                    <samp>
                        #{stderr}
                |]
              _ ->
                mempty

        -- Delete widget.
        deleteW <- do
            -- Check that not on victim team, and it's during the break it round.
            -- if not development && (victim || now < contestBreakFixStart contest || now > contestBreakEnd contest) then
            if True then
                return mempty
              else 
                -- Check if team leader.
                if userId == teamLeader team then
                    -- Check that the break is finished testing. 
                    let st = breakSubmissionStatus bs in
                    if st == BreakPending || st == BreakTesting then
                        return [whamlet'|
                            <h3>
                                Delete
                            <p>
                                You cannot delete a break submission until it is finished being tested.
                        |]
                    else do
                        (deleteW, deleteE) <- handlerToWidget $ generateFormPost deleteBreakSubmissionForm
                        return [whamlet'|
                            <h3>
                                Delete
                            <p>
                                Delete this break submission. 
                            <p .text-danger>
                                Warning: This cannot be undone!
                            <form .form-inline role=form method=post action="@{ParticipationBreakSubmissionDeleteR tcId bsId}" enctype=#{deleteE}>
                                ^{deleteW}
                        |]
                else
                    return [whamlet'|
                        <h3>
                            Delete
                        <p>
                            Only the team leader may delete a break submission. 
                    |]
                    
        -- Show break name if attacker or break-it has ended.
        now <- getCurrentTime
        let name = 
              if not victim || now > contestBreakEnd contest then
                  toHtml $ breakSubmissionName bs 
              else
                  dash

        breakDownloadW <- do
            exists <- (> 0) <$> (handlerToWidget $ runDB $ count [BreakSubmissionFileBreak ==. bsId])
            return $ if exists then
                [whamlet'|
                            #{keyToInt bsId} (<a href="@{ParticipationBreakSubmissionDownloadR tcId bsId}">Download</a>)
                |]
            else
                [whamlet'|
                            #{keyToInt bsId} (Download not available yet)
                |]
        [whamlet|
            <a href="@{ParticipationBreakSubmissionsR tcId}" type="button" class="btn btn-primary">
                Back
            <h2>
                Break Submission
            <form class="form-horizontal">
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Submission ID
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            ^{breakDownloadW}
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Test name
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{name}
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Test type
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{testType}
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Attacking Team
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{attackTeamName} (#{keyToInt (breakSubmissionTeam bs)})
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Target Team
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{targetTeamName} (#{mKeyToInt (breakSubmissionTargetTeam bs)})
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Submission hash
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{breakSubmissionCommitHash bs}
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Timestamp
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{time}
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Status
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{status}
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Result
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{result}
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Message
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            #{message}
                ^{judgementW}
            ^{breakFixesW victim}
            ^{buildOutputW}
            ^{disputeW}
            ^{deleteW}
        |]

        -- Check if we can rerun submission.
        rerunSubmission <- canRerunBreakSubmission bs contest
        when rerunSubmission $ 
            rerunWidget bs

    where

        breakFixesW victim = do
            bfs <- handlerToWidget $ runDB $ [lsql| select BreakFixSubmission.result, FixSubmission.commitHash from BreakFixSubmission left outer join FixSubmission on BreakFixSubmission.fix == FixSubmission.id where (BreakFixSubmission.break == #{bsId}) order by FixSubmission.id desc|]
            let rows = mconcat $ map (breakFixesRow victim) bfs
            [whamlet|
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>
                                Fix commit hash
                            <th>
                                Break result
                    <tbody>
                        ^{rows}
            |]

        breakFixesRow victim (res', commitM) = 
            let res = if victim then prettyBreakResultVictim res' else prettyBreakResult res' in
            [whamlet|
                <tr>
                    <td>
                        #{maybe dash toHtml commitM}
                    <td>
                        #{res}
            |]
            
        rerunWidget bs = do
            ( widget, enctype) <- handlerToWidget $ generateFormPost rerunForm
            [whamlet|
                <h3>
                    Rerun Submission
                <form method=post action="@{ParticipationBreakSubmissionRerunR tcId bsId}" enctype=#{enctype}>
                    ^{widget}
                    <button type="submit" class="btn btn-warning">
                        Rerun
            |]
        
        mKeyToInt Nothing = dash
        mKeyToInt (Just x) = toHtml $ keyToInt x

data DisputeBreakForm = DisputeBreakForm Textarea

disputeBreakSubmissionForm :: Form DisputeBreakForm
disputeBreakSubmissionForm = identifyForm "dispute-break-submission" $ renderBootstrap3 disp $ DisputeBreakForm <$>
       areq textareaField (withPlaceholder "Justification" $ bfs ("Justification" :: Text)) Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Dispute"::Text) "btn btn-primary" [])

    where
        disp = BootstrapInlineForm
        -- disp = BootstrapHorizontalForm (ColMd 0) (ColMd 6) (ColMd 0) (ColMd 4)

postParticipationBreakSubmissionDisputeR :: TeamContestId -> BreakSubmissionId -> Handler ()
postParticipationBreakSubmissionDisputeR tcId bsId = runLHandler $ do
    userId <- requireAuthId
    ((res, widget), enctype) <- runFormPost disputeBreakSubmissionForm
    case res of 
        FormFailure _msg ->
            failureHandler
        FormMissing ->
            failureHandler
        FormSuccess (DisputeBreakForm justificationA) -> do
            -- Get break submission.
            bsM <- runDB $ get bsId
            case bsM of
                Nothing ->
                    notFound
                Just bs ->
                    -- Check that team is target.
                    if breakSubmissionTargetTeam bs /= Just tcId then
                        failureHandler
                    else do
                        -- Check that user is team leader.
                        teamLeaderM <- runDB $ E.select $ E.from $ \(E.InnerJoin (E.InnerJoin tc c) t) -> do
                            E.on (tc E.^. TeamContestTeam E.==. t E.^. TeamId)
                            E.on (tc E.^. TeamContestContest E.==. c E.^. ContestId)
                            E.where_ (tc E.^. TeamContestId E.==. E.val tcId)
                            E.limit 1
                            return (t E.^. TeamLeader, c)
                        case teamLeaderM of
                            [(E.Value leader, (Entity _ contest))] | leader == userId -> do
                                -- Check that it's fix round.
                                now <- getCurrentTime
                                if not development && now > contestBreakEnd contest then
                                    failureHandler
                                else
                                    -- Check the break is accepted.
                                    if breakSubmissionValid bs /= Just True then
                                        failureHandler
                                    else do
                                        runDB $ insert $ BreakDispute bsId $ unTextarea justificationA
                                        setMessage [shamlet|
                                            <div class="container">
                                                <div class="alert alert-success">
                                                    Successfully disputed break submission.
                                        |]
                                        redirect $ ParticipationBreakSubmissionR tcId bsId
                                        
                            _ ->
                                failureHandler
                        
            

    where
        failureHandler = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not dispute break submission.
            |]
            redirect $ ParticipationBreakSubmissionR tcId bsId

data DeleteForm = DeleteForm ()

deleteBreakSubmissionForm :: Form DeleteForm
deleteBreakSubmissionForm = identifyForm "delete-break-submission" $ renderBootstrap3 BootstrapInlineForm $ DeleteForm
    <$> pure ()
    <*  bootstrapSubmit (BootstrapSubmit ("Delete"::Text) "btn btn-danger" [])

postParticipationBreakSubmissionDeleteR :: TeamContestId -> BreakSubmissionId -> Handler ()
postParticipationBreakSubmissionDeleteR tcId bsId = runLHandler $ do
    raiseUserLabel
    ((res, widget), enctype) <- runFormPost deleteBreakSubmissionForm
    case res of
        FormFailure _msg ->
            failureHandler
        FormMissing ->
            failureHandler
        FormSuccess (DeleteForm ()) -> do
            bsM <- runDB $ E.select $ E.from $ \(E.InnerJoin (E.InnerJoin bs tc) t) -> do
                E.on (tc E.^. TeamContestTeam E.==. t E.^. TeamId)
                E.on (tc E.^. TeamContestId E.==. bs E.^. BreakSubmissionTeam)
                E.where_ (bs E.^. BreakSubmissionId E.==. E.val bsId)
                return ( bs, tc E.^. TeamContestContest, t)
            case bsM of
                [((Entity _ bs), (E.Value contestId), (Entity _ team))] -> do
                    -- Get contest.
                    contestM <- runDB $ get contestId
                    case contestM of
                        Nothing ->
                            failureHandler
                        Just contest -> 
                            -- Check that team submitted break. 
                            if breakSubmissionTeam bs /= tcId then
                                failureHandler
                            else do
                                -- Check that it's break round.
                                now <- getCurrentTime
                                if not development && ( now < contestBreakFixStart contest || now > contestBreakEnd contest) then
                                    failureHandler
                                else do
                                    -- Check that user is team leader.
                                    userId <- requireAuthId
                                    if userId /= teamLeader team then
                                        failureHandler
                                    else
                                        -- Check that the break is finished testing.
                                        let st = breakSubmissionStatus bs in
                                        if st == BreakPending || st == BreakTesting then
                                            failureHandler
                                        else do
                                            runDB $ delete bsId
                                            rescoreBreakRound contestId
                                            setMessage [shamlet|
                                                <div class="container">
                                                    <div class="alert alert-success">
                                                        Successfully deleted break submission.
                                            |]
                                            redirect $ ParticipationBreakSubmissionsR tcId
                            
                _ ->
                    failureHandler

    where
        failureHandler = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not delete break submission.
            |]
            redirect $ ParticipationBreakSubmissionR tcId bsId

data RerunFormData = RerunFormData ()

rerunForm = identifyForm "rerun-break-submission" $ renderBootstrap3 BootstrapBasicForm $ RerunFormData
    <$> pure ()

-- Determines if we can rerun submission.
-- JP: We need to be careful with rerunning breaks as this might cause race conditions. 
canRerunBreakSubmission bs contest = do
    (Entity _ user) <- handlerToWidget requireAuth
    if not (userAdmin user) then
        return False
    else
        let status = breakSubmissionStatus bs in
        if status == BreakPending || status == BreakTesting then
            return False
        else if development then
            return True
        else do
            now <- getCurrentTime
            return $ now <= contestBreakEnd contest

postParticipationBreakSubmissionRerunR :: TeamContestId -> BreakSubmissionId -> Handler Html
postParticipationBreakSubmissionRerunR tcId bsId = runLHandler $ do
    bs <- BreakSubmissions.checkBreakSubmissionTeam tcId bsId
    Participation.layout Participation.BreakSubmissions tcId $ \userId teamcontest contest team -> do
        ((res, widget), enctype) <- handlerToWidget $ runFormPost rerunForm
        case res of
            FormMissing ->
                errorHandler
            FormFailure _ ->
                errorHandler
            FormSuccess _ -> do
                -- Check if we can rerun this submission.
                canRerun <- canRerunBreakSubmission bs contest
                if not canRerun then
                    errorHandler
                else do
                    handlerToWidget $ runDB $ update bsId [BreakSubmissionStatus =. BreakPending, BreakSubmissionMessage =. Nothing]

                    setMessage [shamlet|
                        <div .container>
                            <div .alert .alert-success>
                                Rerunning submission.
                    |]
                    redirect $ ParticipationBreakSubmissionR tcId bsId

    where
        errorHandler = do
            setMessage [shamlet|
                <div .container>
                    <div .alert .alert-danger>
                        Could not rerun submission.
            |]
            redirect $ ParticipationBreakSubmissionR tcId bsId
