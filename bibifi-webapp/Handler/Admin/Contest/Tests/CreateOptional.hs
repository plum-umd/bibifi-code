module Handler.Admin.Contest.Tests.CreateOptional where

import qualified Admin
import Import
import Test

generateHtml :: Text -> Widget -> Enctype -> LWidget
generateHtml url form enctype = do
    [whamlet|
        <a href="@{AdminContestTestsR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Create optional test
        <form method=post action="@{AdminContestTestsCreateOptionalR url}" enctype=#{enctype} roles="form">
            ^{form}
            <div .form-group .optional>
                <button .btn .btn-primary type="submit">
                    Create test
    |]

getAdminContestTestsCreateOptionalR :: Text -> Handler Html
getAdminContestTestsCreateOptionalR url = runLHandler $ Admin.layoutContest url $ \_ -> do
    Admin.setTitle "Create test"
    (form, enctype) <- handlerToWidget $ generateFormPost $ testForm Nothing
    generateHtml url form enctype

postAdminContestTestsCreateOptionalR :: Text -> Handler Html
postAdminContestTestsCreateOptionalR url = runLHandler $ Admin.layoutContest url $ \(Entity cId _) -> do
    Admin.setTitle "Create test"
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ testForm Nothing
    case res of
        FormMissing ->
            errorHandler form enctype
        FormFailure _ ->
            errorHandler form enctype
        FormSuccess FormData{..} -> do
            -- Insert test into database.
            handlerToWidget $ runDB $ insert_ $ ContestOptionalTest cId formDataName "" "" $ unTextarea formDataTest

            -- Set message.
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Created test!
            |]
            
            -- Redirect.
            redirect $ AdminContestTestsCreateOptionalR url

    where
        errorHandler form enctype = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not create test.
            |]
            
            generateHtml url form enctype
