module Handler.Admin.Contest.Tests.Optional where

import qualified Admin
import Import
import Test

generateHtml :: Text -> ContestOptionalTestId -> Contest -> Widget -> Enctype -> LWidget
generateHtml url testId contest form enctype = do
    [whamlet|
        <a href="@{AdminContestTestsR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Edit Optional Test
    |]
    
    -- Include warning if contest has started. 
    now <- getCurrentTime
    when (now > contestBuildStart contest) [whamlet|
        <p .text-warning>
            Warning: The contest has started. Previous submissions will not be automatically rerun. 
      |]

    [whamlet|
        <form method=post action="@{AdminContestTestsOptionalR url testId}" enctype=#{enctype} roles="form">
            ^{form}
            <div .form-group .optional>
                <button .btn .btn-primary type="submit">
                    Update Test
    |]

getAdminContestTestsOptionalR :: Text -> ContestOptionalTestId -> Handler Html
getAdminContestTestsOptionalR url testId = runLHandler $ Admin.layoutContest url $ \(Entity contestId contest) -> do
    Admin.setTitle "Edit test"
    test <- handlerToWidget $ runDB $ get404 testId
    when (contestOptionalTestContest test /= contestId)
        notFound
    (form, enctype) <- handlerToWidget $ generateFormPost $ testForm $ Just (contestOptionalTestName test, contestOptionalTestTestScript test)
    generateHtml url testId contest form enctype

postAdminContestTestsOptionalR :: Text -> ContestOptionalTestId -> Handler Html
postAdminContestTestsOptionalR url testId = runLHandler $ Admin.layoutContest url $ \(Entity contestId contest) -> do
    Admin.setTitle "Edit test"
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ testForm Nothing
    case res of
        FormMissing ->
            errorHandler contest form enctype
        FormFailure _ ->
            errorHandler contest form enctype
        FormSuccess FormData{..} -> do
            -- Update test.
            handlerToWidget $ runDB $ update testId [ContestOptionalTestName =. formDataName, ContestOptionalTestTestScript =. unTextarea formDataTest]

            -- Set message.
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Updated test!
            |]
            
            -- Redirect.
            redirect $ AdminContestTestsOptionalR url testId

    where
        errorHandler contest form enctype = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not update test.
            |]
            
            generateHtml url testId contest form enctype


