module Handler.Participation.OracleSubmissions.Create where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as Text
import Data.Time.Clock

import Import
import qualified Participation
import PostDependencyType

data FormData = FormData {
        formName :: Text
      , formInput :: Textarea
    }

form :: Form FormData
form = renderBootstrap3' $ FormData
    <$> areq textField (withPlaceholder "Submission name" $ bootstrapify "Submission") Nothing
    <*> areq textareaField (withPlaceholder "Oracle input" $ bootstrapify "Input") Nothing

generateHtml :: ( Widget, Enctype) -> Maybe Text -> TeamContestId -> Contest -> LWidget
generateHtml ( widget, enctype) msgM tcId contest = do
    now <- lLift $ lift getCurrentTime
    if not development && now < contestBuildStart contest then
        [whamlet|The contest has not started yet.|]
    else 
        let msg = case msgM of
              Nothing -> 
                mempty
              Just msg ->
                displayError msg
        in
        [whamlet|
            ^{msg}
            <form method=post action="@{ParticipationOracleSubmissionCreateR tcId}" enctype=#{enctype} class="form-horizontal" role="form">
                ^{widget}
                <div class="form-group">
                    <div class="col-sm-offset-2 col-sm-10">
                        <button type="submit" class="btn btn-default">
                            Run
        |]

getParticipationOracleSubmissionCreateR :: TeamContestId -> Handler Html
getParticipationOracleSubmissionCreateR tcId = runLHandler $ 
    Participation.layout Participation.Oracle tcId $ \_ _ contest _ -> do
        widgetEnc <- handlerToWidget $ generateFormPost form
        generateHtml widgetEnc Nothing tcId contest

        
postParticipationOracleSubmissionCreateR :: TeamContestId -> Handler Html
postParticipationOracleSubmissionCreateR tcId = runLHandler $ 
    Participation.layout Participation.Oracle tcId $ \_ _ contest _ -> do
        ((res, widget), enctype) <- handlerToWidget $ runFormPost form
        case res of
            FormFailure _msg ->
                generateHtml (widget,enctype) Nothing tcId contest
            FormMissing ->
                generateHtml (widget,enctype) Nothing tcId contest
            FormSuccess (FormData name (Textarea input)) -> do
                -- let input = case Aeson.decodeStrict $ Text.encodeUtf8 input' of
                --       Nothing ->
                --         input'
                --       Just val -> 
                --         let prettyConf = Aeson.defConfig {Aeson.confIndent = 2} in
                --         Text.decodeUtf8 $ BSL.toStrict $ Aeson.encodePretty' prettyConf (val :: Aeson.Value)
                timestamp <- lLift $ lift $ getCurrentTime
                osId <- handlerToWidget $ runDB $ insert $ OracleSubmission tcId timestamp name input Nothing OraclePending
                setMessage [shamlet|
                    <div class="container">
                        <div class="alert alert-success">
                            Oracle submission successful.
                |]
                redirect $ ParticipationOracleSubmissionR tcId osId
        
        [whamlet|TODO|]
