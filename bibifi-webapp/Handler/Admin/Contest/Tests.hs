module Handler.Admin.Contest.Tests where

import qualified Admin
import Import

getAdminContestTestsR :: Text -> Handler Html
getAdminContestTestsR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ case res of 
        Nothing ->
            Admin.contestNotFound
        Just (Entity cId _) -> do
            Admin.setTitle "Tests"
            [whamlet|
                <a href="@{AdminContestR url}" type="button" class="btn btn-primary">
                    Back
                <h2>
                    Tests
            |]
            correctnessWidget cId

            performanceWidget cId

            optionalWidget cId

            clickableDiv


    where
        performanceWidget cId = do
            tests <- handlerToWidget $ runDB $ selectList [ContestPerformanceTestContest ==. cId] []
            [whamlet|
                <h3>
                    Performance Tests
            |]
            case tests of
                [] -> [whamlet|
                    <p>
                        There are currently no performance tests for this contest. <a href="@{AdminContestTestsCreatePerformanceR url}">Create one here</a>.
                  |]
                _ -> do
                    let row (Entity testId test) = 
                          let required = if contestPerformanceTestOptional test then "False" else "True" :: Text in
                          [whamlet'|
                            <tr .clickable href="@{AdminContestTestsPerformanceR url testId}">
                                <td>
                                    #{contestPerformanceTestName test}
                                <td>
                                    #{required}
                          |]
                    let rows = mconcat $ map row tests
                    [whamlet|
                        <p>
                            <a href="@{AdminContestTestsCreatePerformanceR url}">
                                Create a new test.
                        <table class="table table-hover">
                            <thead>
                                <tr>
                                    <th>
                                        Test
                                    <th>
                                        Required
                            <tbody>
                                ^{rows}
                    |]


        optionalWidget cId = do
            tests <- handlerToWidget $ runDB $ selectList [ContestOptionalTestContest ==. cId] []
            [whamlet|
                <h3>
                    Optional Tests
            |]
            case tests of
                [] -> [whamlet|
                    <p>
                        There are currently no optional tests for this contest. <a href="@{AdminContestTestsCreateOptionalR url}">Create one here</a>. 
                  |]
                _ -> do

                    let row (Entity testId test) = [whamlet'|
                            <tr .clickable href="@{AdminContestTestsOptionalR url testId}">
                                <td>
                                    #{contestOptionalTestName test}
                          |]
                    let rows = mconcat $ map row tests
                    [whamlet|
                        <p>
                            <a href="@{AdminContestTestsCreateOptionalR url}">
                                Create a new test.
                        <table class="table table-hover">
                            <thead>
                                <tr>
                                    <th>
                                        Test
                            <tbody>
                                ^{rows}
                    |]

        correctnessWidget cId = do
            correctnessTests <- handlerToWidget $ runDB $ selectList [ContestCoreTestContest ==. cId] []
            [whamlet|
                <h3>
                    Correctness Tests
            |]
            case correctnessTests of
                [] -> [whamlet|
                    <p>
                        There are currently no correctness tests for this contest. <a href="@{AdminContestTestsCreateCorrectnessR url}">Create one here</a>. 
                  |]
                _ -> do
                    
                    let row (Entity testId test) = [whamlet'|
                            <tr .clickable href="@{AdminContestTestsCorrectnessR url testId}">
                                <td>
                                    #{contestCoreTestName test}
                          |]
                    let rows = mconcat $ map row correctnessTests
                    [whamlet|
                        <p>
                            <a href="@{AdminContestTestsCreateCorrectnessR url}">
                                Create a new test.
                        <table class="table table-hover">
                            <thead>
                                <tr>
                                    <th>
                                        Test
                            <tbody>
                                ^{rows}
                    |]

