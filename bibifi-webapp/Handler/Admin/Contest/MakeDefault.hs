module Handler.Admin.Contest.MakeDefault where

import Import
import qualified Admin    

data Default = Default Text

defaultForm :: Text -> Form Default
defaultForm url = renderDivs $ Default
    <$> pure url

generateHtml :: Text -> Widget -> Enctype -> [Text] -> LHandler Html
generateHtml url widget enctype msg = do
    Admin.layout Admin.Contests $ do
        -- Get contest.
        res <- handlerToWidget $ retrieveContest $ Just url
        case res of
            Nothing -> do
                Admin.contestNotFound
            Just entity -> do
                let Entity _ c = entity
                Admin.setTitle $ contestTitle c
                -- Check if this is already the default contest.
                isDefault <- handlerToWidget $ isDefaultContest
                [whamlet|
                    <h2>
                        Default
                        <small>
                            #{contestTitle c}
                |]
                if isDefault entity then
                    [whamlet|
                        This is already the default contest.
                    |]
                else do
                    let msgH = mconcat $ map displayError msg
                    [whamlet|
                        ^{msgH}
                        <form method=post action=@{MakeDefaultR url} enctype=#{enctype}>
                            ^{widget}
                            <input type=submit class="btn btn-primary" value="Make default contest">
                    |]

getMakeDefaultR :: Text -> Handler Html
getMakeDefaultR url = runLHandler $ do
    ( widget, enctype) <- generateFormPost $ defaultForm url
    generateHtml url widget enctype []

postMakeDefaultR :: Text -> Handler Html
postMakeDefaultR url' = runLHandler $ do
    raiseUserLabel
    ((res, widget), enctype) <- runFormPost $ defaultForm url'
    case res of 
        FormSuccess (Default url) -> do
            -- Check that the contest exists..
            res' <- retrieveContest $ Just url
            case res' of 
                Nothing ->
                    generateHtml url widget enctype ["This contest does not exist."]
                Just _ -> do
                    setConfig DefaultContest url
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Successfully set the default contest!
                    |]
                    redirect $ AdminContestR url
        FormFailure _msg ->
            generateHtml url' widget enctype []
        FormMissing ->
            generateHtml url' widget enctype []

