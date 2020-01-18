module Handler.Admin.Contest.Judgements.AssignBreak where

import Control.Applicative

import qualified Admin
import qualified BreakSubmissions
import Import
import qualified Judges

data FormData = FormData JudgeId

form :: Maybe (Entity BreakJudgement) -> [(Text,JudgeId)] -> Form FormData
form judgementM judges = 
    let defJudge = maybe Nothing (\(Entity _ j) -> Just (breakJudgementJudge j)) judgementM in
    renderBootstrap3 disp $ FormData
        <$> areq (selectFieldList judges) (bfs ("Judge"::Text)) defJudge
            where
                disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)

generateForm :: ContestId -> Maybe (Entity BreakJudgement) -> BreakSubmissionId -> LHandler (Form FormData)
generateForm cId judgementM bsId = runDB $ do
    -- Get list of judges.
    judges <- Judges.getOrdered cId bsId
    return $ form judgementM judges

generateView :: Entity Contest -> BreakSubmissionId -> Maybe (Entity BreakJudgement) -> Widget -> Enctype -> [Text] -> LWidget
generateView (Entity cId c) bsId judgementM formW enctype msg = 
    error "TODO"
    -- let msgH = mconcat $ map displayError msg in
    -- let ( ruling, comments, buttonText) = case judgementM of
    --       Nothing ->
    --         ( dash, dash, "Assign" :: Text)
    --       Just (Entity _ judgement) -> 
    --         let ruling = maybe dash (\r -> if r then [shamlet|Passed|] else [shamlet|Failed|]) $ breakJudgementRuling judgement in
    --         let comments = maybe dash (\c -> [shamlet|#{c}|]) $ breakJudgementComments judgement in
    --         ( ruling, comments, "Reassign")
    -- in
    -- do
    -- ( team, targetTeam, testName, hash, language) <- handlerToWidget $ runDB $ do
    --     -- res <- E.select $ E.from $ \( E.InnerJoin t (E.InnerJoin tc (E.InnerJoin bs (E.InnerJoin tct tt)))) -> do
    --     --     E.on ( tct E.^. TeamContestTeam E.==. tt E.^. TeamId)
    --     --     E.on ( bs E.^. BreakSubmissionTargetTeam E.==. tct E.^. TeamContestId)
    --     --     E.on ( bs E.^. BreakSubmissionTeam E.==. tc E.^. TeamContestId)
    --     --     E.on ( tc E.^. TeamContestTeam E.==. t E.^. TeamId)
    --     --     E.where_ ( bs E.^. BreakSubmissionId E.==. E.val bsId)
    --     --     E.limit 1
    --     res <- BreakSubmissions.getBothTeams bsId $ \ (Entity _ t) _tc (Entity _ bs) (Entity _ tct) (Entity _ tt) ->
    --         ( teamName t, teamName tt, breakSubmissionName bs, breakSubmissionCommitHash bs, teamContestLanguages tct)
    --     return $ case res of
    --         Just ( team, targetTeam, testName, hash, language) ->
    --             let toHamlet t = [shamlet|#{t}|] in
    --             ( toHamlet team, toHamlet targetTeam, toHamlet testName, toHamlet hash, toHamlet language)
    --         _ ->
    --             ( dash, dash, dash, dash, dash)
    -- [whamlet|
    --     <a href="@{AdminContestJudgementsR (contestUrl c)}" type="button" class="btn btn-primary">
    --         Back
    --     <h2>
    --         Judgements
    --         <small>
    --             #{contestTitle c}
    --     ^{msgH}
    --     <form method=post action="@{AdminContestJudgementsAssignBreakR (contestUrl c) bsId}" enctype=#{enctype} class="form-horizontal" role="form">
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Team
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{team}
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Target Team
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{targetTeam}
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Test name
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{testName}
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Language
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{language}
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Submission hash
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{hash}
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Submission id
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{keyToInt bsId}
    --         ^{formW}
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Ruling
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{ruling}
    --         <div .form-group>
    --             <label .col-md-3 .control-label>
    --                 Comments
    --             <div .col-md-9>
    --                 <p .form-control-static>
    --                     #{comments}
    --         <div .form-group .optional>
    --             <div .col-md-offset-3 .col-md-9>
    --                 <button .btn .btn-primary type="submit">
    --                     #{buttonText}
    -- |]

getAdminContestJudgementsAssignBreakR :: Text -> BreakSubmissionId -> Handler Html
getAdminContestJudgementsAssignBreakR url bsId = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just c'@(Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                -- Get old judgement.
                judgementM <- handlerToWidget $ runDB $ getBy $ UniqueBreakJudgement bsId
                judgeForm <- handlerToWidget $ generateForm cId judgementM bsId
                ( widget, enctype) <- handlerToWidget $ generateFormPost judgeForm
                generateView c' bsId judgementM widget enctype []
    
postAdminContestJudgementsAssignBreakR :: Text -> BreakSubmissionId -> Handler Html
postAdminContestJudgementsAssignBreakR url bsId = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just c'@(Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                -- Get old judgement.
                judgementM <- handlerToWidget $ runDB $ getBy $ UniqueBreakJudgement bsId
                judgeForm <- handlerToWidget $ generateForm cId judgementM bsId
                ((res, widget), enctype) <- handlerToWidget $ runFormPost judgeForm
                case res of
                    FormFailure _msg ->
                        generateView c' bsId judgementM widget enctype []
                    FormMissing ->
                        generateView c' bsId judgementM widget enctype []
                    FormSuccess (FormData judgeId) -> do
                        handlerToWidget $ runDB $ do
                            case judgementM of
                                Nothing ->
                                    insert_ $ BreakJudgement judgeId bsId Nothing Nothing
                                Just (Entity oldJudgementId oldJudgement) -> do
                                    update oldJudgementId [BreakJudgementJudge =. judgeId]
                                    Judges.decrementCount $ breakJudgementJudge oldJudgement
                            Judges.incrementCount judgeId
                        setMessage [shamlet|
                            <div class="container">
                                <div class="alert alert-success">
                                    Successfully assigned judge!
                        |]
                        redirect $ AdminContestJudgementsR url
