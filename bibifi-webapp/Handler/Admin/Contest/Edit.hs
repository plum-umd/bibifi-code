module Handler.Admin.Contest.Edit where

import qualified Admin
import Contest.Edit
import Import

generateHtml url form enctype errMsgM = do
    let errMsg = maybe mempty displayError errMsgM
    [whamlet|
        <a href="@{AdminContestR url}" type="button" class="btn btn-primary">
            Back
        <h2>
            Edit Contest
        ^{errMsg}
        <form method=post action="@{AdminContestEditR url}" enctype="#{enctype}" role="form">
            ^{form}
            <div class="form-group optional">
                <button class="btn btn-primary" type="submit">
                    Edit Contest
    |]

getAdminContestEditR :: Text -> Handler Html
getAdminContestEditR url = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "Edit Contest"
    tz <- liftIO $ contestTimeZone
    (Entity _ contest) <- getContest url
    (form, enctype) <- handlerToWidget $ generateFormPost $ contestForm tz $ Just contest
    generateHtml url form enctype Nothing

postAdminContestEditR :: Text -> Handler Html
postAdminContestEditR url = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "Edit Contest"
    (Entity contestId oldContest) <- getContest url
    tz <- liftIO $ contestTimeZone
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ contestForm tz $ Just oldContest
    case res of
        FormMissing -> 
            errorHandler url form enctype Nothing
        FormFailure _ ->
            errorHandler url form enctype Nothing
        FormSuccess dat -> do
            -- Validate form.
            valid <- validateContest dat $ Just contestId
            case valid of
                Left msg ->
                    errorHandler url form enctype $ Just msg
                Right () -> handlerToWidget $ do
                    -- Update contest.
                    let newContest = convertContest dat
                    runDB $ replace contestId newContest

                    -- Check if url has changed. If so and it was the default, update the default. 
                    when (contestUrl oldContest /= contestUrl newContest) $ do
                        currentDefaultUrl <- getConfig DefaultContest
                        when (Just (contestUrl oldContest) == currentDefaultUrl) $
                            setConfig DefaultContest $ contestUrl newContest

                    -- Set message.
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Updated contest!
                    |]

                    -- Redirect.
                    redirect $ AdminContestEditR $ contestUrl newContest

    where
        errorHandler url form enctype msgM = generateHtml url form enctype msgM

getContest url = handlerToWidget $ do
    res <- retrieveContest $ Just url
    case res of 
        Nothing ->
            -- Admin.contestNotFound
            notFound
        Just contest ->
            return contest
