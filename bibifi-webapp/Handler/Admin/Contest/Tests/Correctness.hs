module Handler.Admin.Contest.Tests.Correctness where

import qualified Admin
import Import
import Test

generateHtml :: Text -> ContestCoreTestId -> Contest -> Widget -> Enctype -> LWidget
generateHtml url testId contest form enctype = do
    [whamlet|
        <a href="@{AdminContestTestsR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Edit Correctness Test
    |]
    -- Include warning if contest has started. 
    now <- getCurrentTime
    when (now > contestBuildStart contest) [whamlet|
        <p .text-warning>
            Warning: The contest has started. Previous submissions will not be automatically rerun. 
      |]
    [whamlet|
        <form method=post action="@{AdminContestTestsCorrectnessR url testId}" enctype=#{enctype} roles="form">
            ^{form}
            <div .form-group .optional>
                <button .btn .btn-primary type="submit">
                    Update Test
    |]

getAdminContestTestsCorrectnessR :: Text -> ContestCoreTestId -> Handler Html
getAdminContestTestsCorrectnessR url testId = runLHandler $ Admin.layoutContest url $ \(Entity contestId contest) -> do
    Admin.setTitle "Edit test"
    test <- handlerToWidget $ runDB $ get404 testId
    when (contestCoreTestContest test /= contestId)
        notFound
    (form, enctype) <- handlerToWidget $ generateFormPost $ testForm $ Just (contestCoreTestName test, contestCoreTestTestScript test)
    generateHtml url testId contest form enctype

postAdminContestTestsCorrectnessR :: Text -> ContestCoreTestId -> Handler Html
postAdminContestTestsCorrectnessR url testId = runLHandler $ Admin.layoutContest url $ \(Entity _ contest) -> do
    Admin.setTitle "Edit test"
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ testForm Nothing
    case res of
        FormMissing ->
            errorHandler contest form enctype
        FormFailure _ ->
            errorHandler contest form enctype
        FormSuccess FormData{..} -> do
            -- Update test.
            handlerToWidget $ runDB $ update testId $ [ContestCoreTestName =. formDataName, ContestCoreTestTestScript =. unTextarea formDataTest]

            -- Set message.
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Updated test!
            |]
            
            -- Redirect.
            redirect $ AdminContestTestsCorrectnessR url testId

    where
        errorHandler contest form enctype = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not update test.
            |]
            
            generateHtml url testId contest form enctype


-- JP: Delete form? Need to be careful due to propogation... Check if contest has started???
