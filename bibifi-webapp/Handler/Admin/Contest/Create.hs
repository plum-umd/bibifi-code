module Handler.Admin.Contest.Create where

import Control.Monad.Trans.Except (throwE, runExceptT)
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

generateHtml form enctype errMsgM = do
    let errMsg = maybe mempty displayError errMsgM
    [whamlet|
        <a href="@{AdminContestsR}" type="button" class="btn btn-primary">
            Back
        <h2>
            Create new contest
        ^{errMsg}
        <form method=post action="@{AdminContestCreateR}" enctype="#{enctype}" role="form">
            ^{form}
    |]
    

getAdminContestCreateR :: Handler Html
getAdminContestCreateR = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "New Contest"
    tz <- liftIO $ contestTimeZone
    (form, enctype) <- handlerToWidget $ generateFormPost $ contestForm tz
    generateHtml form enctype Nothing

postAdminContestCreateR :: Handler Html
postAdminContestCreateR = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "New Contest"
    tz <- liftIO $ contestTimeZone
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ contestForm tz
    case res of
        FormMissing -> 
            errorHandler form enctype Nothing
        FormFailure _ ->
            errorHandler form enctype Nothing
        FormSuccess dat -> do
            valid <- validateContest dat
            case valid of
                Left msg -> 
                    errorHandler form enctype $ Just msg
                Right () -> do
                    -- Convert and insert contest.
                    _ <- handlerToWidget $ runDB $ insert $ convertContest dat

                    -- Set this as the default contest.
                    handlerToWidget $ setConfig DefaultContest $ formContestUrl dat

                    -- Set message.
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Created contest!
                    |]

                    -- Redirect.
                    redirect $ AdminContestR $ formContestUrl dat

    where
        errorHandler form enctype msgM = do
            generateHtml form enctype msgM

        convertContest FormData{..} = Contest
            formContestUrl
            formContestName
            formBuildStart
            formBuildEnd
            formBreakStart
            formBreakEnd
            formFixStart
            formFixEnd
            
        validateContest FormData{..} = runExceptT $ do
            -- Check that url is unique. 
            do
                contestM <- lift $ handlerToWidget $ runDB $ getBy $ UniqueContest formContestUrl
                whenJust contestM $ 
                    throwE "This URL is already taken."

            -- Check that all dates are sequential.
            when (formBuildStart >= formBuildEnd) $ 
                throwE "Build-it end date must be after the start date."

            when (formBuildEnd >= formBreakStart) $ 
                throwE "Break-it start date must be after the build-it end date."

            when (formBreakStart >= formBreakEnd) $
                throwE "Break-it end date must be after the start date."

            when (formBreakEnd >= formFixStart) $
                throwE "Fix-it start date must be after the break-it end date."

            when (formFixStart >= formFixEnd) $
                throwE "Fix-it end date must be after the start date."

            return ()

