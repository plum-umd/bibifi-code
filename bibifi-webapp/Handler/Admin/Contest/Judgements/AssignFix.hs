module Handler.Admin.Contest.Judgements.AssignFix where

import Control.Applicative

import qualified Admin
import qualified BreakSubmissions
import Import
import qualified Judges

data FormData = FormData JudgeId

form :: Maybe (Entity FixJudgement) -> [(Text,JudgeId)] -> Form FormData
form judgementM judges = 
    let defJudge = maybe Nothing (\(Entity _ j) -> Just (fixJudgementJudge j)) judgementM in
    renderBootstrap3 disp $ FormData
        <$> areq (selectFieldList judges) (bfs ("Judge"::Text)) defJudge
            where
                disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)

generateForm :: ContestId -> Maybe (Entity FixJudgement) -> FixSubmissionId -> LHandler (Form FormData)
generateForm cId judgementM fsId = runDB $ do
    -- Get list of judges.
    judges <- Judges.getOrdered cId fsId
    return $ form judgementM judges

generateView :: Entity Contest -> FixSubmissionId -> Maybe (Entity FixJudgement) -> Widget -> Enctype -> [Text] -> LWidget
generateView (Entity cId c) fsId judgementM formW enctype msg = 
    let msgH = mconcat $ map displayError msg in
    let ( ruling, comments, buttonText) = case judgementM of
          Nothing ->
            ( dash, dash, "Assign" :: Text)
          Just (Entity _ judgement) -> 
            let ruling = maybe dash (\r -> if r then [shamlet|Passed|] else [shamlet|Failed|]) $ fixJudgementRuling judgement in
            let comments = maybe dash (\c -> [shamlet|#{c}|]) $ fixJudgementComments judgement in
            ( ruling, comments, "Reassign")
    in
    do
    ( team, hash, language) <- do
        res <- handlerToWidget $ runDB [lsql| 
                select Team.name, FixSubmission.commitHash, TeamContest.languages from Team
                inner join TeamContest on Team.id == TeamContest.team
                inner join FixSubmission on FixSubmission.team == TeamContest.id
                where FixSubmission.id == #{fsId}
                limit 1
            |]
        -- $ E.select $ E.from $ \(E.InnerJoin fs (E.InnerJoin tc t)) -> do
        --     E.on (tc E.^. TeamContestTeam E.==. t E.^. TeamId)
        --     E.on (fs E.^. FixSubmissionTeam E.==. tc E.^. TeamContestId)
        --     E.where_ (fs E.^. FixSubmissionId E.==. E.val fsId)
        --     E.limit 1
        --     return ( t E.^. TeamName, fs E.^. FixSubmissionName, fs E.^. FixSubmissionCommitHash, tc E.^. TeamContestLanguages)
        return $ case res of 
            [(team', hash', language')] ->
                let toHamlet t = [shamlet|#{t}|] in
                ( toHamlet team', toHamlet hash', toHamlet language')
            _ ->
                ( dash, dash, dash)
    [whamlet|
        <a href="@{AdminContestJudgementsR (contestUrl c)}" type="button" class="btn btn-primary">
            Back
        <h2>
            Judgements
            <small>
                #{contestTitle c}
        ^{msgH}
        <form method=post action="@{AdminContestJudgementsAssignFixR (contestUrl c) fsId}" enctype=#{enctype} class="form-horizontal" role="form">
            <div .form-group>
                <label .col-md-3 .control-label>
                    Team
                <div .col-md-9>
                    <p .form-control-static>
                        #{team}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Language
                <div .col-md-9>
                    <p .form-control-static>
                        #{language}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Submission hash
                <div .col-md-9>
                    <p .form-control-static>
                        #{hash}
            ^{formW}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Ruling
                <div .col-md-9>
                    <p .form-control-static>
                        #{ruling}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Comments
                <div .col-md-9>
                    <p .form-control-static>
                        #{comments}
            <div .form-group .optional>
                <div .col-md-offset-3 .col-md-9>
                    <button .btn .btn-primary type="submit">
                        #{buttonText}
    |]

getAdminContestJudgementsAssignFixR :: Text -> FixSubmissionId -> Handler Html
getAdminContestJudgementsAssignFixR url fsId = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just c'@(Entity cId c) -> do
                -- Get old judgement.
                judgementM <- handlerToWidget $ runDB $ getBy $ UniqueFixJudgement fsId
                judgeForm <- handlerToWidget $ generateForm cId judgementM fsId
                ( widget, enctype) <- handlerToWidget $ generateFormPost judgeForm
                generateView c' fsId judgementM widget enctype []

postAdminContestJudgementsAssignFixR :: Text -> FixSubmissionId -> Handler Html
postAdminContestJudgementsAssignFixR url fsId = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just c'@(Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                -- Get old judgement.
                judgementM <- handlerToWidget $ runDB $ getBy $ UniqueFixJudgement fsId
                judgeForm <- handlerToWidget $ generateForm cId judgementM fsId
                ((res, widget), enctype) <- handlerToWidget $ runFormPost judgeForm
                case res of
                    FormFailure _msg ->
                        generateView c' fsId judgementM widget enctype []
                    FormMissing ->
                        generateView c' fsId judgementM widget enctype []
                    FormSuccess (FormData judgeId) -> do
                        handlerToWidget $ runDB $ do
                            case judgementM of
                                Nothing ->
                                    insert_ $ FixJudgement judgeId fsId Nothing Nothing
                                Just (Entity oldJudgementId oldJudgement) -> do
                                    update oldJudgementId [FixJudgementJudge =. judgeId]
                                    Judges.decrementCount $ fixJudgementJudge oldJudgement
                            Judges.incrementCount judgeId
                        setMessage [shamlet|
                            <div class="container">
                                <div class="alert alert-success">
                                    Successfully assigned judge!
                        |]
                        redirect $ AdminContestJudgementsR url
