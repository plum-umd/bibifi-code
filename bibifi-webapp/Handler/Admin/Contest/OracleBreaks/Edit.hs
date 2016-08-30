module Handler.Admin.Contest.OracleBreaks.Edit where

import Control.Applicative ((<*))
import qualified Database.Esqueleto as E
import Score

import qualified Admin
import Forms
import Import

data FormData = FormData {
      formTeam :: TeamContestId
    , formDescription :: Maybe Textarea
    , formValid :: Bool
    }

form :: [( Text, TeamContestId)] -> BreakOracleSubmission -> Form FormData
form teams bos = renderBootstrap3 disp $ FormData
    <$> areq (selectFieldList teams) (bfs ("Team" :: Text)) (Just $ breakOracleSubmissionTeam bos)
    <*> aopt textareaField (withPlaceholder "Description" (bfs ("Description" :: Text))) (Just $ Just $ Textarea $ breakOracleSubmissionDescription bos)
    <*> areq boolField' (bfs' ("Valid" :: Text)) (Just $ breakOracleSubmissionValid bos)
    <*  bootstrapSubmit (BootstrapSubmit ("Update"::Text) "btn-primary" [])

    where
        disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)
        

generateHtml :: BreakOracleSubmissionId -> Widget -> Enctype -> Contest -> LHandler Html
generateHtml bosId widget enctype c = Admin.layout Admin.Contests $ do
            Admin.setTitle $ contestTitle c
            [whamlet|
                <a href="@{AdminContestOracleBreaksR (contestUrl c)}" type="button" class="btn btn-primary">
                    Back
                <h2>
                    Edit Oracle Break
                    <small>
                        #{contestTitle c}
                <p>
                    Award a team break-it points for finding a bug in the oracle. 
                    The first team to find the bug should be awarded the points. 

                <form method=post action=@{AdminContestOracleBreaksEditR bosId} enctype=#{enctype} class="form-horizontal">
                    ^{widget}
            |]

getBreakOracleAndContest :: BreakOracleSubmissionId -> LHandler [(Entity BreakOracleSubmission, Entity Contest)]
getBreakOracleAndContest bosId = runDB $ E.select $ E.from $ \(E.InnerJoin (E.InnerJoin c tc) bos) -> do
    E.on (tc E.^. TeamContestId E.==. bos E.^. BreakOracleSubmissionTeam)
    E.on (c E.^. ContestId E.==. tc E.^. TeamContestContest)
    E.where_ (bos E.^. BreakOracleSubmissionId E.==. E.val bosId)
    return (bos, c)
    

getAdminContestOracleBreaksEditR :: BreakOracleSubmissionId -> Handler Html
getAdminContestOracleBreaksEditR bosId = runLHandler $ do
    raiseUserLabel
    contestM <- getBreakOracleAndContest bosId
    case contestM of
        [((E.Entity _ bos), (E.Entity cId contest))] -> do
            teams <- generateTeamList cId
            (widget, enctype) <- generateFormPost $ form teams bos
            generateHtml bosId widget enctype contest
        _ ->
            notFound

postAdminContestOracleBreaksEditR :: BreakOracleSubmissionId -> Handler Html
postAdminContestOracleBreaksEditR bosId = runLHandler $ do
    raiseUserLabel
    contestM <- getBreakOracleAndContest bosId
    case contestM of
        [((E.Entity _ bos), (E.Entity cId contest))] -> do
            teams <- generateTeamList cId
            ((res, widget), enctype) <- runFormPost $ form teams bos
            case res of
                FormSuccess (FormData tcId descM valid) -> do
                    let desc = maybe "" unTextarea descM
                    let time = breakOracleSubmissionTimestamp bos

                    -- Update in DB.
                    runDB $ replace bosId $ BreakOracleSubmission tcId time desc valid

                    -- Mark for rescore. 
                    rescoreBuildRound cId

                    -- Set message. 
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Successfully updated oracle break. 
                    |]

                    -- Redirect.
                    redirect $ AdminContestOracleBreaksR $ contestUrl contest

                FormFailure _msg ->
                    generateHtml bosId widget enctype contest
                FormMissing ->
                    generateHtml bosId widget enctype contest

        _ ->
            notFound
    
