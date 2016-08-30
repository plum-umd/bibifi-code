module Handler.Profile.Account.Upload where

import Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.Binary

import Forms
import Import
import qualified Profile

data FormData = FormData FileInfo

data FormType = ConsentForm | Resume

-- uploadForm :: Text -> Form FormData
uploadForm field = renderBootstrap3 disp $ FormData
    <$> field
    <*  bootstrapSubmit (BootstrapSubmit ("Upload"::Text) "btn-primary" [])
        where
            disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 6)

consentForm :: Form FormData
consentForm = uploadForm $ areq 
    (uploadField [".jpg",".jpeg",".png",".pdf"])
    ((bfs' "Consent Form") {fsAttrs=[], fsTooltip=Just "Must be a pdf, jpg, or png file less than 4MB."}) Nothing

resumeForm :: Form FormData
resumeForm = uploadForm $ areq
    (uploadField [".txt",".doc",".docx",".pdf"])
    ((bfs' "Resume") {fsAttrs=[], fsTooltip=Just "Must be a doc, docx, txt, or pdf file less than 4MB."}) Nothing

generateHtmlConsent :: Widget -> Enctype -> [Text] -> LWidget
generateHtmlConsent widget enctype msg =
    let msgH = mconcat $ map displayError msg in
    [whamlet|
        <a href="@{ProfileAccountR}" type="button" class="btn btn-primary">
            Back
        <h3>
            Consent Form
        <p>
            Fill out, sign, and upload <a href="@{StaticR doc_consent_form_pdf}">this form</a>.
        ^{msgH}
        <form method=post action="@{ProfileAccountConsentR}" enctype=#{enctype} class="form-horizontal" role="form">
            ^{widget}
    |]

generateHtmlResume :: Widget -> Enctype -> [Text] -> LWidget
generateHtmlResume widget enctype msg =
    let msgH = mconcat $ map displayError msg in
    -- TODO: update this text for "Please..."
    lLift $ [whamlet'|
        <a href="@{ProfileAccountR}" type="button" class="btn btn-primary">
            Back
        <h3>
            Resume
        <p>
            Please upload your resume here.
        ^{msgH}
        <form method=post action="@{ProfileAccountResumeR}" enctype=#{enctype} class="form-horizontal" role="form">
            ^{widget}
    |]

handleFormResult :: FormType -> ([Text] -> LWidget) -> UserId -> FormResult FormData -> LWidget
handleFormResult formtype generateHtml uId res = do
    let field = case formtype of
                  ConsentForm -> UserConsentForm
                  Resume      -> UserResume
    let succtext = case formtype of
                     ConsentForm -> "consent form"::Text
                     Resume      -> "resume"
    case res of
        FormSuccess (FormData fileInfo) -> do
            fileBytes <- do
                bytes <- runResourceT $ fileSource fileInfo $$ sinkLbs
                return $ BL.toStrict bytes
            -- Check that the file is not too big.
            if BS.length fileBytes > 4000000 then
                generateHtml ["File is too large."]
            else do
                handlerToWidget $ runDB $ do
                    fileId <- insert $ StoredFile (Just uId) (fileName fileInfo) (fileContentType fileInfo) $ fileBytes
                    updateWhere [UserId ==. uId] [field =. Just fileId]
                    -- E.update $ \u -> do
                    --     E.set u [ field E.=. E.val ( Just fileId)]
                    --     E.where_ ( u E.^. UserId E.==. E.val uId)
                setMessage [shamlet|$newline never
                    <div class="container">
                        <div class="alert alert-success">
                            Successfully uploaded #{succtext}.
                |]
                redirect ProfileAccountR
        _ ->
            generateHtml []

getProfileAccountConsentR :: Handler Html
getProfileAccountConsentR = runLHandler $ Profile.layout Profile.Account $ \_ -> do
    ( widget, enctype) <- handlerToWidget $ generateFormPost consentForm
    generateHtmlConsent widget enctype []

postProfileAccountConsentR :: Handler Html
postProfileAccountConsentR = runLHandler $ Profile.layout Profile.Account $ \uId -> do
    ((res, widget), enctype) <- handlerToWidget $ runFormPost $ consentForm
    handleFormResult ConsentForm (generateHtmlConsent widget enctype) uId res

getProfileAccountResumeR :: Handler Html
getProfileAccountResumeR = runLHandler $ Profile.layout Profile.Account $ \_ -> do
    ( widget, enctype) <- handlerToWidget $ generateFormPost resumeForm
    generateHtmlResume widget enctype []

postProfileAccountResumeR :: Handler Html
postProfileAccountResumeR = runLHandler $ Profile.layout Profile.Account $ \uId -> do
    ((res, widget), enctype) <- handlerToWidget $ runFormPost $ resumeForm
    handleFormResult Resume (generateHtmlResume widget enctype) uId res
