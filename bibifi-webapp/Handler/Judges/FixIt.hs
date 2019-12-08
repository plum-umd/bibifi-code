module Handler.Judges.FixIt where

import Control.Applicative
import Control.Monad

import Forms
import Import
import qualified Judges
import PostDependencyType
import Score

data FormData = FormData Bool (Maybe Textarea)

form :: FixJudgement -> Form FormData
form j = 
    let defRul = fixJudgementRuling j in
    let defComm = maybe 
          Nothing 
          (Just . Just . Textarea) $ fixJudgementComments j
    in
    renderBootstrap3 disp $ FormData
        <$> areq boolField' "All break tests are the same bug and it's fixed" defRul
        <*> aopt textareaField "Comments" defComm
        <*  bootstrapSubmit (BootstrapSubmit ("Submit"::Text) "btn-primary" [])
            where
                disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)

redirectUnauthorized :: JudgeId -> FixJudgement -> LWidget
redirectUnauthorized jId j = when (jId /= fixJudgementJudge j) $ do
    (Entity _ u) <- handlerToWidget requireAuth
    unless (userAdmin u)
        notFound

generateView :: Text -> FixJudgementId -> FixSubmissionId -> FixSubmission -> Widget -> Enctype -> [Text] -> LWidget
generateView url jId fId fix formW enctype msg = 
    let msgH = mconcat $ map displayError msg in
    do
    return undefined -- FIXME FixSubmissionBugs table gone
    {-breaks <- do
        bs <- handlerToWidget $ runDB [lsql| select TeamContest.id, BreakSubmission.name from BreakSubmission inner join FixSubmissionBugs on FixSubmissionBugs.bugId == BreakSubmission.id inner join TeamContest on TeamContest.id == BreakSubmission.team where FixSubmissionBugs.fix == #{fId} |]
        
        
        -- $ E.select $ E.from $ \(E.InnerJoin fb (E.InnerJoin bs tc)) -> do
        --     E.on ( tc E.^. TeamContestId E.==. bs E.^. BreakSubmissionTeam)
        --     E.on ( fb E.^. FixSubmissionBugsBugId E.==. bs E.^. BreakSubmissionId)
        --     E.where_ ( fb E.^. FixSubmissionBugsFix E.==. E.val fId)
        --     return ( tc E.^. TeamContestId, bs E.^. BreakSubmissionName)
        let dispFirst (tcId, bsName) = [whamlet'|
                <div .form-group>
                    <label .col-md-3 .control-label>
                        Break tests
                    <p .col-md-9 .form-control-static>
                        /repos/#{keyToInt tcId}/break/#{bsName}/
              |]
        return $ case bs of 
            [] ->
                [whamlet'|
                    <div .form-group>
                        <label .col-md-3 .control-label>
                            Break tests
                        <p .col-md-9 .form-control-static>
                            #{dash}
                |]
            [h] ->
                dispFirst h
            h:t -> 
                let dispTest (tcId, bsName) = [whamlet'|
                        <div .form-group>
                            <label .col-md-3 .control-label>
                                
                            <p .col-md-9 .form-control-static>
                                /repos/#{keyToInt tcId}/break/#{bsName}/
                      |]
                in
                mconcat $ (dispFirst h):(map dispTest t)
    [whamlet|
        <a href="@{JudgesR url}" type="button" class="btn btn-primary">
            Back
        ^{msgH}
        <form method=post action="@{JudgesFixItR url jId}" enctype=#{enctype} class="form-horizontal" role="form">
            <div .form-group>
                <label .col-md-3 .control-label>
                    Job id
                <p .col-md-9 .form-control-static>
                    fix-#{keyToInt jId}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Repository path
                <p .col-md-9 .form-control-static>
                    /repos/#{keyToInt (fixSubmissionTeam fix)}/fix/#{fixSubmissionName fix}/
            <div .form-group>
                <label .col-md-3 .control-label>
                    Commit hash
                <p .col-md-9 .form-control-static>
                    #{fixSubmissionCommitHash fix}
            ^{breaks}
            ^{formW}
    |]-}

getJudgesFixItR :: Text -> FixJudgementId -> Handler Html
getJudgesFixItR url jId = runLHandler $ 
    Judges.layout url $ \uId _ judgeId -> do
        judgementM <- handlerToWidget $ runDB $ get jId
        case judgementM of
            Nothing ->
                notFound
            Just judgement -> do
                redirectUnauthorized judgeId judgement
                ( widget, enctype) <- handlerToWidget $ generateFormPost $ form judgement
                let fixId = fixJudgementSubmission judgement
                fixM <- handlerToWidget $ runDB $ get fixId
                case fixM of
                    Nothing ->
                        notFound
                    Just fix ->
                        generateView url jId fixId fix widget enctype []

postJudgesFixItR :: Text -> FixJudgementId -> Handler Html
postJudgesFixItR url jId = runLHandler $
    Judges.layout url $ \uId (Entity cId _) judgeId -> do
        judgementM <- handlerToWidget $ runDB $ get jId
        case judgementM of
            Nothing ->
                notFound
            Just judgement -> do
                redirectUnauthorized judgeId judgement
                let fixId = fixJudgementSubmission judgement
                fixM <- handlerToWidget $ runDB $ get fixId
                case fixM of
                    Nothing ->
                        notFound
                    Just fix -> do
                        ((res, widget), enctype) <- handlerToWidget $ runFormPost $ form judgement
                        case res of 
                            FormFailure _msg ->
                                generateView url jId fixId fix widget enctype []
                            FormMissing ->
                                generateView url jId fixId fix widget enctype []
                            FormSuccess (FormData passed commentsM) ->
                                let comments = maybe Nothing (Just . unTextarea) commentsM in
                                let (newResult,newRuling) = 
                                      if passed then 
                                        (FixFixed, Just True)
                                      else
                                        (FixNotFixed, Just False)
                                in
                                handlerToWidget $ do
                                    runDB $ update fixId [FixSubmissionStatus =. FixJudged, FixSubmissionResult =. Just newResult]
                                    runDB $ update jId [FixJudgementRuling =. newRuling, FixJudgementComments =. comments]
                                    -- Rescore fix round.
                                    rescoreFixRound cId
                        setMessage [shamlet|
                            <div class="container">
                                <div class="alert alert-success">
                                    Successfully submitted judgement!
                        |]
                        redirect $ JudgesR url
