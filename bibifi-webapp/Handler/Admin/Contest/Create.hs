module Handler.Admin.Contest.Create where

import Data.Time (TimeZone)

import Import
import qualified Admin
import Forms (utcField)

data FormData = FormData {
    formContestName :: Text
  , formContestUrl :: Text
  , formBuildStart :: UTCTime
  , formBuildEnd :: UTCTime
  , formBreakStart :: UTCTime
  , formBreakEnd :: UTCTime
  , formFixStart :: UTCTime
  , formFixEnd :: UTCTime
  -- , formMakeDefault :: Bool
  -- TODO: Coursera corse and session? XXX
}

-- contestForm :: TimeZone -> Form FormData
contestForm tz = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField nameSettings Nothing
    <*> areq urlField urlSettings Nothing
    <*> areq (utcField tz) buildStartSettings Nothing
    <*> areq (utcField tz) buildEndSettings Nothing
    <*> areq (utcField tz) breakStartSettings Nothing
    <*> areq (utcField tz) breakEndSettings Nothing
    <*> areq (utcField tz) fixStartSettings Nothing
    <*> areq (utcField tz) fixEndSettings Nothing
    <* bootstrapSubmit (BootstrapSubmit ("Create Contest"::Text) "btn-primary" [])

    where
        nameSettings = withPlaceholder "Contest Name" $ bfs ("Contest Name" :: Text)
        urlSettings = withPlaceholder "Contest URL" $ bfs ("Contest URL" :: Text) -- TODO: Verify placeholder is unique. XXX
        buildStartSettings = withPlaceholder "Build-it Start Date" $ bfs ("Build-it Start Date" :: Text)
        buildEndSettings = withPlaceholder "Build-it End Date" $ bfs ("Build-it End Date" :: Text)
        breakStartSettings = withPlaceholder "Break-it Start Date" $ bfs ("Break-it Start Date" :: Text)
        breakEndSettings = withPlaceholder "Break-it End Date" $ bfs ("Break-it End Date" :: Text)
        fixStartSettings = withPlaceholder "Fix-it Start Date" $ bfs ("Fix-it Start Date" :: Text)
        fixEndSettings = withPlaceholder "Fix-it End Date" $ bfs ("Fix-it End Date" :: Text)

        -- urlField = checkM (\url -> do
        --     contestM <- runDB $ getBy $ UniqueContest url  
        --     return $ maybe (Left ("This URL is already taken" :: Text)) (const $ Right url) contestM
        --   ) textField
        urlField = textField

generateHtml form enctype = do
    [whamlet|
        <h1>
            Create new contest
        <form method=post action="@{AdminContestCreateR}" enctype="#{enctype}" role="form">
            ^{form}
    |]
    

getAdminContestCreateR :: Handler Html
getAdminContestCreateR = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "New Contest"
    tz <- liftIO $ contestTimeZone
    (form, enctype) <- handlerToWidget $ generateFormPost $ contestForm tz
    generateHtml form enctype

postAdminContestCreateR :: Handler Html
postAdminContestCreateR = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "New Contest"
    [whamlet|TODO|]

