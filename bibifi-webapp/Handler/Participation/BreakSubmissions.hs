module Handler.Participation.BreakSubmissions where

import Data.Maybe
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
        -- submissions <- handlerToWidget $ runDB $ [lsql| select BreakSubmission.*, Team.name from BreakSubmission left outer join TeamContest on BreakSubmission.targetTeam == TeamContest.id inner join Team on TeamContest.team == Team.id where BreakSubmission.team == #{tcId} order by BreakSubmission.id desc |]
        -- TODO: Fix the above query..
        submissions <- handlerToWidget $ runDB $ do
            subs <- [lsql| select BreakSubmission.* from BreakSubmission where BreakSubmission.team == #{tcId} order by BreakSubmission.id desc |]
            mapM (\bsE@(Entity _ bs) -> case breakSubmissionTargetTeam bs of
                Nothing ->
                    return (bsE, Nothing)
                Just targetTeamId -> do
                    names <- [lsql| select Team.name from TeamContest inner join Team on TeamContest.team == Team.id where TeamContest.id == #{targetTeamId} limit 1|]
                    return (bsE, listToMaybe names)
              ) subs
            
        
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
                    displayBreakSubmissionsTable contest BreakSubmissionVictim $ fmap (fmap Just) ss
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
            res <- handlerToWidget $ runDB $ BreakSubmissions.getBothTeams bsId $ \(Entity _ t) _tc _bs _tct tt ->
                ( teamName t, fmap entityVal tt)
            case res of
                Just names ->
                    return names
                _ ->
                    notFound

        let targetTeamName = maybe dash (toHtml . teamName) targetTeam

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
                      if Just userId /= fmap teamLeader targetTeam then
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

        -- Withdraw widget.
        withdrawW <- do
            -- Check that not on victim team, and it's during the break it round.
            if not development && (victim || now < contestBreakFixStart contest || now > contestBreakEnd contest || breakSubmissionWithdrawn bs == True) then
            -- if True then
                return mempty
              else 
                -- Check if team leader.
                if userId == teamLeader team then
                    -- Check that the break is finished testing. 
                    let st = breakSubmissionStatus bs in
                    if st == BreakPending || st == BreakTesting then
                        return [whamlet'|
                            <h3>
                                Withdraw
                            <p>
                                You cannot withdraw a break submission until it is finished being tested.
                        |]
                    else do
                        (withdrawW, withdrawE) <- handlerToWidget $ generateFormPost withdrawBreakSubmissionForm
                        return [whamlet'|
                            <h3>
                                Withdraw
                            <p>
                                Withdraw this break submission. 
                            <p .text-danger>
                                Warning: This cannot be undone!
                            <form .form-inline role=form method=post action="@{ParticipationBreakSubmissionWithdrawR tcId bsId}" enctype=#{withdrawE}>
                                ^{withdrawW}
                        |]
                else
                    return [whamlet'|
                        <h3>
                            Withdraw
                        <p>
                            Only the team leader may withdraw a break submission. 
                    |]
                    
        -- -- Show break name if attacker or break-it has ended.
        -- now <- getCurrentTime
        -- let name = 
        --       if not victim || now > contestBreakEnd contest then
        --           toHtml $ breakSubmissionName bs 
        --       else
        --           dash
        let name = breakSubmissionName bs

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
                ^{withdrawnW bs}
                ^{judgementW}
            ^{breakFixesW victim (breakSubmissionTimestamp bs)}
            ^{buildOutputW}
            ^{disputeW}
            ^{withdrawW}
        |]

        -- Check if we can rerun submission.
        rerunSubmission <- canRerunBreakSubmission bs contest
        when rerunSubmission $ 
            rerunWidget bs

    where
        withdrawnW bs = when (breakSubmissionWithdrawn bs) [whamlet|
                <div class="form-group">
                    <label class="col-xs-3 control-label">
                        Withdrawn
                    <div class="col-xs-9">
                        <p class="form-control-static">
                            True
            |]

        breakFixesW victim breakTime = do
            bfs <- handlerToWidget $ runDB $ [lsql| select BreakFixSubmission.result, FixSubmission.timestamp, FixSubmission.commitHash from BreakFixSubmission left outer join FixSubmission on BreakFixSubmission.fix == FixSubmission.id where (BreakFixSubmission.break == #{bsId}) order by FixSubmission.id desc|]
            let rows = mconcat $ map (breakFixesRow victim breakTime) bfs
            [whamlet|
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>
                                Fix commit hash
                            <th>
                                Fix timestamp
                            <th>
                                Break result
                    <tbody>
                        ^{rows}
            |]

        breakFixesRow victim breakTime (res', fixTimeM, commitM) = do
            let res = if victim then prettyBreakResultVictim res' else prettyBreakResult res'
            time <- displayTime $ maybe breakTime id fixTimeM

            [whamlet|
                <tr>
                    <td>
                        #{maybe dash toHtml commitM}
                    <td>
                        #{time}
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

data WithdrawForm = WithdrawForm ()

withdrawBreakSubmissionForm :: Form WithdrawForm
withdrawBreakSubmissionForm = identifyForm "withdraw-break-submission" $ renderBootstrap3 BootstrapInlineForm $ WithdrawForm
    <$> pure ()
    <*  bootstrapSubmit (BootstrapSubmit ("Withdraw"::Text) "btn btn-danger" [])

postParticipationBreakSubmissionWithdrawR :: TeamContestId -> BreakSubmissionId -> Handler ()
postParticipationBreakSubmissionWithdrawR tcId bsId = runLHandler $ do
    raiseUserLabel
    ((res, widget), enctype) <- runFormPost withdrawBreakSubmissionForm
    case res of
        FormFailure _msg ->
            failureHandler
        FormMissing ->
            failureHandler
        FormSuccess (WithdrawForm ()) -> do
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
                                        if st == BreakPending || st == BreakTesting || breakSubmissionWithdrawn bs == True then
                                            failureHandler
                                        else do
                                            runDB $ BreakSubmissions.withdrawBreakSubmission bsId
                                            rescoreBreakRound contestId
                                            setMessage [shamlet|
                                                <div class="container">
                                                    <div class="alert alert-success">
                                                        Successfully withdrew break submission.
                                            |]
                                            redirect $ ParticipationBreakSubmissionsR tcId
                            
                _ ->
                    failureHandler

    where
        failureHandler = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not withdraw break submission.
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
        if status == BreakPending || status == BreakTesting || breakSubmissionWithdrawn bs == True then
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
