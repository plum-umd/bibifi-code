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

            clickableDiv


    where
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
                        <table class="table table-hover">
                            <thead>
                                <tr>
                                    <th>
                                        Test
                            <tbody>
                                ^{rows}
                    |]

