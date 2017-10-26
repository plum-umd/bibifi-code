module Handler.Admin.Contest.Tests.Performance where

import qualified Admin
import Import
import Test

generateHtml :: Text -> ContestPerformanceTestId -> Contest -> Widget -> Enctype -> LWidget
generateHtml url testId contest form enctype = do
    [whamlet|
        <a href="@{AdminContestTestsR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Edit Performance Test
    |]
    -- Include warning if contest has started. 
    now <- getCurrentTime
    when (now > contestBuildStart contest) [whamlet|
        <p .text-warning>
            Warning: The contest has started. Previous submissions will not be automatically rerun. 
      |]
    [whamlet|
        <form method=post action="@{AdminContestTestsPerformanceR url testId}" enctype=#{enctype} roles="form">
            ^{form}
            <div .form-group .optional>
                <button .btn .btn-primary type="submit">
                    Update Test
    |]

getAdminContestTestsPerformanceR :: Text -> ContestPerformanceTestId -> Handler Html
getAdminContestTestsPerformanceR url testId = runLHandler $ Admin.layoutContest url $ \(Entity contestId contest) -> do
    Admin.setTitle "Edit test"
    test <- handlerToWidget $ runDB $ get404 testId
    when (contestPerformanceTestContest test /= contestId)
        notFound
    (form, enctype) <- handlerToWidget $ generateFormPost $ performanceTestForm $ Just (contestPerformanceTestName test, contestPerformanceTestTestScript test, not (contestPerformanceTestOptional test))
    generateHtml url testId contest form enctype

postAdminContestTestsPerformanceR :: Text -> ContestPerformanceTestId -> Handler Html
postAdminContestTestsPerformanceR url testId = runLHandler $ Admin.layoutContest url $ \(Entity contestId contest) -> do
    Admin.setTitle "Edit test"
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ performanceTestForm Nothing
    case res of
        FormMissing ->
            errorHandler contest form enctype
        FormFailure _ ->
            errorHandler contest form enctype
        FormSuccess PerformanceFormData{..} -> do
            -- Update test.
            handlerToWidget $ runDB $ update testId $ [ContestPerformanceTestName =. performanceFormDataName, ContestPerformanceTestTestScript =. unTextarea performanceFormDataTest, ContestPerformanceTestOptional =. not (performanceFormDataRequired)]

            -- Set message.
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Updated test!
            |]
            
            -- Redirect.
            redirect $ AdminContestTestsPerformanceR url testId

    where
        errorHandler contest form enctype = do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Could not update test.
            |]
            
            generateHtml url testId contest form enctype

