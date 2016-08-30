module Handler.Admin.Contest.Judgements.AssignBuild where

import Control.Applicative

import qualified Admin
import Import
import qualified Judges

data FormData = FormData JudgeId

form :: Maybe (Entity BuildJudgement) -> [(Text,JudgeId)] -> Form FormData
form judgementM judges = 
    let defJudge = maybe Nothing (\(Entity _ j) -> Just (buildJudgementJudge j)) judgementM in
    renderBootstrap3 disp $ FormData
        <$> areq (selectFieldList judges) (bfs ("Judge"::Text)) defJudge
            where
                disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)

generateView :: Entity Contest -> BuildSubmissionId -> Maybe (Entity BuildJudgement) -> Widget -> Enctype -> [Text] -> LWidget
generateView (Entity cId c) bsId judgementM formW enctype msg = 
    let msgH = mconcat $ map displayError msg in
    let ( ruling, comments, buttonText) = case judgementM of
          Nothing ->
            ( dash, dash, "Assign" :: Text)
          Just (Entity _ judgement) -> 
            let ruling = maybe dash (\r -> if r then [shamlet|Passed|] else [shamlet|Failed|]) $ buildJudgementRuling judgement in
            let comments = maybe dash (\c -> [shamlet|#{c}|]) $ buildJudgementComments judgement in
            ( ruling, comments, "Reassign")
    in
    do
    ( team, hash, language) <- handlerToWidget $ runDB $ do
        res <- [lsql|
                select Team.name, BuildSubmission.commitHash, TeamContest.languages from Team
                inner join TeamContest on TeamContest.team == Team.id
                inner join BuildSubmission on BuildSubmission.team == TeamContest.id
                where BuildSubmission.id == #{bsId}
                limit 1
            |]
        
        -- E.select $ E.from $ \( E.InnerJoin t (E.InnerJoin tc bs)) -> do
        --     E.on ( bs E.^. BuildSubmissionTeam E.==. tc E.^. TeamContestId)
        --     E.on ( tc E.^. TeamContestTeam E.==. t E.^. TeamId)
        --     E.where_ ( bs E.^. BuildSubmissionId E.==. E.val bsId)
        --     E.limit 1
        --     return ( t E.^. TeamName, bs E.^. BuildSubmissionCommitHash, tc E.^. TeamContestLanguages)
        return $ case res of
            [(team, hash, language)] ->
                let toHamlet t = [shamlet|#{t}|] in
                ( toHamlet team, toHamlet hash, toHamlet language)
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
        <form method=post action="@{AdminContestJudgementsAssignBuildR (contestUrl c) bsId}" enctype=#{enctype} class="form-horizontal" role="form">
            <div .form-group>
                <label .col-md-3 .control-label>
                    Team
                <div .col-md-9>
                    <p .form-control-static>
                        #{team}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Submission hash
                <div .col-md-9>
                    <p .form-control-static>
                        #{hash}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Submission id
                <div .col-md-9>
                    <p .form-control-static>
                        #{keyToInt bsId}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Language
                <div .col-md-9>
                    <p .form-control-static>
                        #{language}
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

generateForm :: ContestId -> Maybe (Entity BuildJudgement) -> BuildSubmissionId -> LHandler (Form FormData)
generateForm cId judgementM bsId = runDB $ do
    -- Get list of judges.
    judges <- Judges.getOrdered cId bsId
    return $ form judgementM judges

getAdminContestJudgementsAssignBuildR :: Text -> BuildSubmissionId -> Handler Html
getAdminContestJudgementsAssignBuildR url bsId = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just c'@(Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                -- Get old judgement.
                judgementM <- handlerToWidget $ runDB $ getBy $ UniqueBuildJudgement bsId
                judgeForm <- handlerToWidget $ generateForm cId judgementM bsId
                ( widget, enctype) <- handlerToWidget $ generateFormPost $ judgeForm
                generateView c' bsId judgementM widget enctype []

postAdminContestJudgementsAssignBuildR :: Text -> BuildSubmissionId -> Handler Html
postAdminContestJudgementsAssignBuildR url bsId = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just c'@(Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                -- Get old judgement.
                judgementM <- handlerToWidget $ runDB $ getBy $ UniqueBuildJudgement bsId
                judgeForm <- handlerToWidget $ generateForm cId judgementM bsId
                ((res, widget), enctype) <- handlerToWidget $ runFormPost $ judgeForm
                case res of
                    FormFailure _msg ->
                        generateView c' bsId judgementM widget enctype []
                    FormMissing ->
                        generateView c' bsId judgementM widget enctype []
                    FormSuccess (FormData judgeId) -> do
                        handlerToWidget $ runDB $ do
                            case judgementM of
                                Nothing ->
                                    insert_ $ BuildJudgement judgeId bsId Nothing Nothing
                                Just (Entity oldJudgementId oldJudgement) -> do
                                    update oldJudgementId [BuildJudgementJudge =. judgeId]
                                    Judges.decrementCount $ buildJudgementJudge oldJudgement
                            Judges.incrementCount judgeId
                        setMessage [shamlet|
                            <div class="container">
                                <div class="alert alert-success">
                                    Successfully assigned judge!
                        |]
                        redirect $ AdminContestJudgementsR url
