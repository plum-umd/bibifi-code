module Handler.Judges.BuildIt where

import Control.Applicative
import Control.Monad

import Forms
import Import
import qualified Judges
import Score

data FormData = FormData Bool (Maybe Textarea)

form :: BuildJudgement -> Form FormData
form j = 
    let defObf = maybe Nothing (Just . not) $ buildJudgementRuling j in
    let defComm = maybe 
          Nothing 
          (Just . Just . Textarea) $ buildJudgementComments j
    in
    renderBootstrap3 disp $ FormData
--        <$> areq (radioFieldList [("Not obfuscated"::Text,False),("Obfuscated",True)]) "Judgment" Nothing
        <$> areq boolField' "Obfuscated" defObf
        <*> aopt textareaField "Comments" defComm
        <*  bootstrapSubmit (BootstrapSubmit ("Submit"::Text) "btn-primary" [])
            where
                disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)

generateView :: Text -> BuildJudgementId -> Widget -> Enctype -> [Text] -> LWidget
generateView url jId formW enctype msg = 
    let msgH = mconcat $ map displayError msg in
    [whamlet|
        <a href="@{JudgesR url}" type="button" class="btn btn-primary">
            Back
        ^{msgH}
        <form method=post action="@{JudgesBuildItR url jId}" enctype=#{enctype} class="form-horizontal" role="form">
            <div .form-group>
                <label .col-md-3 .control-label>
                    Job id
                <p .col-md-9 .form-control-static>
                    build-#{keyToInt jId}
            <div .form-group>
                <label .col-md-3 .control-label>
                    Repository path
                <p .col-md-9 .form-control-static>
                    /build/#{keyToInt jId}
            ^{formW}
    |]

redirectUnauthorized :: JudgeId -> BuildJudgement -> LWidget
redirectUnauthorized jId j = when (jId /= buildJudgementJudge j) $ do
    (Entity _ u) <- handlerToWidget requireAuth
    unless (userAdmin u)
        notFound

getJudgesBuildItR :: Text -> BuildJudgementId -> Handler Html
getJudgesBuildItR url jId = runLHandler $
    Judges.layout url $ \uId _ judgeId -> do
        judgementM <- handlerToWidget $ runDB $ get jId
        case judgementM of
            Nothing ->
                notFound
            Just judgement -> do
                redirectUnauthorized judgeId judgement
                ( widget, enctype) <- handlerToWidget $ generateFormPost $ form judgement
                generateView url jId widget enctype []

postJudgesBuildItR :: Text -> BuildJudgementId -> Handler Html
postJudgesBuildItR url jId = runLHandler $ do
    Judges.layout url $ \uId (Entity cId _) judgeId -> do
        judgementM <- handlerToWidget $ runDB $ get jId
        case judgementM of
            Nothing ->
                notFound
            Just judgement -> do
                redirectUnauthorized judgeId judgement
                ((res, widget), enctype) <- handlerToWidget $ runFormPost $ form judgement
                case res of
                    FormFailure _msg ->
                        generateView url jId widget enctype []
                    FormMissing ->
                        generateView url jId widget enctype []
                    FormSuccess (FormData obfuscated commentsM) ->
                        let ruling = Just $ not obfuscated in
                        let comments = maybe Nothing (Just . unTextarea) commentsM in
                        do
                        handlerToWidget $ runDB $ update jId [BuildJudgementRuling =. ruling, BuildJudgementComments =. comments]
                        handlerToWidget $ rescoreBuildRound cId
                        setMessage [shamlet|
                            <div class="container">
                                <div class="alert alert-success">
                                    Successfully submitted judgement!
                        |]
                        redirect $ JudgesR url
