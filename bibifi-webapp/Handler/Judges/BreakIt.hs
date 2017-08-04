module Handler.Judges.BreakIt where

import Control.Applicative
import Control.Monad

import Forms
import Import
import qualified Judges
import PostDependencyType
import Score

data BreakJudgementRuling = BreakRulingReject | BreakRulingBug | BreakRulingVulnerability
    deriving (Eq)
data FormData = FormData BreakJudgementRuling (Maybe Textarea)

form :: BreakJudgement -> BreakSubmission -> Form FormData
form j bs = 
    let defJudgement = case breakJudgementRuling j of
          Just True -> Just $ case breakSubmissionResult bs of
            Just BreakExploit -> 
                BreakRulingVulnerability
            Just BreakCorrect ->
                BreakRulingBug
            _ ->
                BreakRulingReject
          Just False ->
            Just BreakRulingReject
          Nothing ->
            Nothing
    in
    let defComm = maybe 
          Nothing 
          (Just . Just . Textarea) $ breakJudgementComments j
    in
    renderBootstrap3 disp $ FormData
        <$> areq (selectFieldList [
                ("Invalid break test"::Text,BreakRulingReject),
                ("Valid bug",BreakRulingBug),
                ("Valid vulnerability",BreakRulingVulnerability)
            ]) (bfs ("Judgement"::Text)) defJudgement
        <*> aopt textareaField "Comments" defComm
        <*  bootstrapSubmit (BootstrapSubmit ("Submit"::Text) "btn-primary" [])
            where
                disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)

generateView :: Text -> BreakJudgementId -> BreakSubmission -> Maybe BreakDispute -> Widget -> Enctype -> [Text] -> LWidget
generateView url jId bs bdM formW enctype msg = do
    let msgH = mconcat $ map displayError msg
    let bayesianScore = case breakSubmissionBayesianScore bs of 
          Nothing ->
            dash
          Just s ->
            [shamlet|#{s}|]
    let dispute = case bdM of
            Nothing ->
                dash
            Just bd ->
                [shamlet|#{breakDisputeJustification bd}|]
    targetTeamJId <- do
        -- Depends on the invariant that each build submission has an assigned judgement.
        res <- handlerToWidget $ runDB [lsql| select BuildJudgement.id from BuildJudgement inner join BuildSubmission on BuildSubmission.id == BuildJudgement.submission where BuildSubmission.team == #{breakSubmissionTargetTeam bs} order by BuildSubmission.timestamp desc limit 1|]
        -- E.select $ E.from $ \( E.InnerJoin bus j) -> do
        --     E.on ( bus E.^. BuildSubmissionId E.==. j E.^. BuildJudgementSubmission)
        --     E.where_ ( bus E.^. BuildSubmissionTeam E.==. E.val (breakSubmissionTargetTeam bs))
        --     E.orderBy [E.desc (bus E.^. BuildSubmissionTimestamp)]
        --     E.limit 1
        --     return ( j E.^. BuildJudgementId)
        return $ case res of
            [bjId] ->
                keyToInt bjId
            _ ->
                -1
    let breakType = case breakSubmissionBreakType bs of
            Nothing ->
                dash
            Just typ ->
                prettyBreakType typ
    [whamlet|
        <a href="@{JudgesR url}" type="break" class="btn btn-primary">
            Back
        ^{msgH}
        <form method=post action="@{JudgesBreakItR url jId}" enctype=#{enctype} class="form-horizontal" role="form">
            <div .form-group>
                <label .col-md-3 .control-label>
                    Job id
                <p .col-md-9 .form-control-static>
                    break-#{keyToInt jId}
            <div .form-group>
                <label .col-md-3 .control-label>
                    BreakType
                <p .col-md-9 .form-control-static>
                    #{breakType}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Repository location
                <p .col-md-9 .form-control-static>
                    /break/#{keyToInt (breakSubmissionTeam bs)}/#{breakSubmissionName bs}.*
            <div .form-group>
                <label .col-md-3 .control-label>
                    Target repository location
                <p .col-md-9 .form-control-static>
                    /build/#{targetTeamJId}/build
            <div .form-group>
                <label .col-md-3 .control-label>
                    Bayesian score
                <p .col-md-9 .form-control-static>
                    #{bayesianScore}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Dispute
                <p .col-md-9 .form-control-static>
                    #{dispute}
            ^{formW}
    |]

redirectUnauthorized :: JudgeId -> BreakJudgement -> LWidget
redirectUnauthorized jId j = when (jId /= breakJudgementJudge j) $ do
    (Entity _ u) <- handlerToWidget requireAuth
    unless (userAdmin u)
        notFound

getJudgesBreakItR :: Text -> BreakJudgementId -> Handler Html
getJudgesBreakItR url jId = runLHandler $ do
    Judges.layout url $ \uId _ judgeId -> do
        judgementM <- handlerToWidget $ runDB $ get jId
        case judgementM of
            Nothing ->
                notFound
            Just judgement -> do
                redirectUnauthorized judgeId judgement
                let bsId = breakJudgementSubmission judgement
                bsM <- handlerToWidget $ runDB $ get bsId
                case bsM of 
                    Nothing ->
                        notFound
                    Just bs -> do
                        bdM <- fmap (fmap entityVal) $ handlerToWidget $ runDB $ getBy $ UniqueBreakDispute bsId
                        ( widget, enctype) <- handlerToWidget $ generateFormPost $ form judgement bs
                        generateView url jId bs bdM widget enctype []

postJudgesBreakItR :: Text -> BreakJudgementId -> Handler Html
postJudgesBreakItR url jId = runLHandler $ do
    Judges.layout url $ \uId (Entity cId _) judgeId -> do
        judgementM <- handlerToWidget $ runDB $ get jId
        case judgementM of
            Nothing ->
                notFound
            Just judgement -> do
                redirectUnauthorized judgeId judgement
                let bsId = breakJudgementSubmission judgement
                bsM <- handlerToWidget $ runDB $ get bsId
                case bsM of 
                    Nothing ->
                        notFound
                    Just bs -> do
                        bdM <- fmap (fmap entityVal) $ handlerToWidget $ runDB $ getBy $ UniqueBreakDispute bsId
                        ((res, widget), enctype) <- handlerToWidget $ runFormPost $ form judgement bs
                        case res of
                            FormFailure _msg ->
                                generateView url jId bs bdM widget enctype []
                            FormMissing ->
                                generateView url jId bs bdM widget enctype []
                            FormSuccess (FormData ruling commentsM) ->
                                let comments = maybe Nothing (Just . unTextarea) commentsM in
                                let newResult = Just $ case ruling of
                                      BreakRulingReject ->
                                        BreakIncorrect
                                      BreakRulingBug ->
                                        BreakCorrect
                                      BreakRulingVulnerability ->
                                        BreakExploit
                                in
                                let newRuling = case ruling of
                                      BreakRulingReject ->
                                        Just False
                                      BreakRulingBug ->
                                        Just True
                                      BreakRulingVulnerability ->
                                        Just True
                                in
                                do
                                handlerToWidget $ do
                                    -- Update to judged, pass or fail
                                    runDB $ update (breakJudgementSubmission judgement) [BreakSubmissionStatus =. BreakJudged, BreakSubmissionResult =. newResult]
                                    -- Set ruling, update comments.
                                    runDB $ update jId [BreakJudgementRuling =. newRuling, BreakJudgementComments =. comments]
                                    -- Rescore break round.
                                    rescoreBreakRound cId
                                setMessage [shamlet|
                                    <div class="container">
                                        <div class="alert alert-success">
                                            Successfully submitted judgement!
                                |]
                                redirect $ JudgesR url
