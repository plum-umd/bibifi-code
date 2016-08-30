module Handler.Admin.Contest.OracleBreaks.Create where

import Control.Applicative ((<*))
import qualified Data.Time.Clock as Clock
import Score

import qualified Admin
import Forms
import Import

data FormData = FormData {
      formTeam :: TeamContestId
    , formDescription :: Maybe Textarea
    }

form :: [( Text, TeamContestId)] -> Form FormData
form teams = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq (selectFieldList teams) (bfs ("Team" :: Text)) Nothing
    <*> aopt textareaField (withPlaceholder "Description" (bfs ("Description" :: Text))) Nothing
    <*  bootstrapSubmit (BootstrapSubmit ("Create"::Text) "btn-primary" [])

generateHtml :: Text -> Widget -> Enctype -> Contest -> LHandler Html
generateHtml url widget enctype c = Admin.layout Admin.Contests $ do
            Admin.setTitle $ contestTitle c
            [whamlet|
                <a href="@{AdminContestOracleBreaksR url}" type="button" class="btn btn-primary">
                    Back
                <h2>
                    New Oracle Break
                    <small>
                        #{contestTitle c}
                <p>
                    Award a team break-it points for finding a bug in the oracle. 
                    The first team to find the bug should be awarded the points. 

                <form method=post action=@{AdminContestOracleBreaksCreateR url} enctype=#{enctype}>
                    ^{widget}
            |]

getAdminContestOracleBreaksCreateR :: Text -> Handler Html
getAdminContestOracleBreaksCreateR url = runLHandler $ do
    raiseUserLabel
    res <- retrieveContest $ Just url
    case res of 
        Nothing ->
            notFound
        Just (Entity cId contest) -> do
            teams <- generateTeamList cId
            (widget, enctype) <- generateFormPost $ form teams
            generateHtml url widget enctype contest
    
postAdminContestOracleBreaksCreateR :: Text -> Handler Html
postAdminContestOracleBreaksCreateR url = runLHandler $ do
    raiseUserLabel
    res <- retrieveContest $ Just url
    case res of 
        Nothing ->
            notFound
        Just (Entity cId contest) -> do
            teams <- generateTeamList cId
            ((res, widget), enctype) <- runFormPost $ form teams
            case res of
                FormSuccess (FormData tcId descM) -> do
                    -- Prepare data.
                    let desc = maybe "" unTextarea descM
                    now <- liftIO $ Clock.getCurrentTime

                    -- Insert into DB.
                    runDB $ insert_ $ BreakOracleSubmission tcId now desc True

                    -- Mark for rescore. 
                    rescoreBuildRound cId

                    -- Set message. 
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Successfully added oracle break. 
                    |]

                    -- Redirect.
                    redirect $ AdminContestOracleBreaksR url
                FormFailure _msg ->
                    generateHtml url widget enctype contest
                FormMissing ->
                    generateHtml url widget enctype contest

