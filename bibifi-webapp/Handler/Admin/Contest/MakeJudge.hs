module Handler.Admin.Contest.MakeJudge where

import Import
import qualified Admin

data FormData = FormData UserId

form :: [(Text, UserId)] -> Form FormData
form users = renderBootstrap3 disp $ FormData
    <$> areq (selectFieldList users) (bfs ("User" :: Text)) Nothing
    <*  bootstrapSubmit (BootstrapSubmit ("Make Judge" :: Text) "btn-primary" [])

    where
        -- disp = BootstrapHorizontalForm (ColMd 0) (ColMd 3) (ColMd 0) (ColMd 9)
        disp = BootstrapBasicForm

generateForm :: LHandler (Form FormData)
generateForm = do
    users <- runDB $ selectList [] []
    return $ form $ map (\(Entity uId u) -> (userIdent u, uId)) users
    

generateHtml :: Maybe Text -> Widget -> Enctype -> Entity Contest -> LWidget
generateHtml msgM formW enctype (Entity _cId c) = do
    Admin.setTitle $ contestTitle c
    [whamlet|
        <a href="@{AdminContestR url}" type="button" class="btn btn-primary">
            Back
        <h2>
            Add Judge
            <small>
                #{contestTitle c}
        ^{maybe mempty displayError msgM}
        <form method=post action="@{AdminContestMakeJudgeR url}" enctype=#{enctype} role="form">
            ^{formW}
        
    |]

    where
        url = contestUrl c


getAdminContestMakeJudgeR :: Text -> Handler Html
getAdminContestMakeJudgeR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ case res of
        Nothing ->
            Admin.contestNotFound
        Just cE@(Entity _ c) -> do
            (widget, enctype) <- handlerToWidget $ generateForm >>= generateFormPost
            generateHtml Nothing widget enctype cE

postAdminContestMakeJudgeR :: Text -> Handler Html
postAdminContestMakeJudgeR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ case res of
        Nothing ->
            Admin.contestNotFound
        Just cE@(Entity cId c) -> do
            ((res, widget), enctype) <- handlerToWidget $ generateForm >>= runFormPost
            case res of
                FormSuccess (FormData userId) -> do
                    -- Insert judge.
                    resM <- handlerToWidget $ runDB $ insertUnique $ Judge userId cId 0

                    -- Check if the user is already a judge.
                    case resM of
                        Nothing ->
                            generateHtml (Just "This user is already a judge.") widget enctype cE
                        Just _ -> do
                            -- Set message.
                            setMessage [shamlet|
                                <div class="container">
                                    <div class="alert alert-success">
                                        Successfully added judge. 
                            |]

                            -- Redirect.
                            redirect $ AdminContestMakeJudgeR url
                FormFailure _msg ->
                    generateHtml Nothing widget enctype cE
                FormMissing ->
                    generateHtml Nothing widget enctype cE

