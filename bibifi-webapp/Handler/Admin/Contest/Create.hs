module Handler.Admin.Contest.Create where

import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Time (TimeZone)

import Import
import qualified Admin
import Contest.Edit


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
            <div class="form-group optional">
                <button class="btn btn-primary" type="submit">
                    Create Contest
    |]
    

getAdminContestCreateR :: Handler Html
getAdminContestCreateR = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "New Contest"
    tz <- liftIO $ contestTimeZone
    (form, enctype) <- handlerToWidget $ generateFormPost $ contestForm tz Nothing
    generateHtml form enctype Nothing

postAdminContestCreateR :: Handler Html
postAdminContestCreateR = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "New Contest"
    tz <- liftIO $ contestTimeZone
    ((res, form), enctype) <- handlerToWidget $ runFormPost $ contestForm tz Nothing
    case res of
        FormMissing -> 
            errorHandler form enctype Nothing
        FormFailure _ ->
            errorHandler form enctype Nothing
        FormSuccess dat -> do
            valid <- validateContest dat Nothing
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

