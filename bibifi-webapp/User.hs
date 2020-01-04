module User where

import Import
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (addUTCTime)
import Network.Mail.Mime hiding (randomString)
import qualified Text.Blaze.Html.Renderer.String as S (renderHtml)
import qualified Text.Blaze.Html.Renderer.Utf8 as U (renderHtml)
import Text.Shakespeare.Text (stext)
import Yesod.Auth.HashDB (setPassword)

sendResetCode :: Text -> Text -> LHandler ()
sendResetCode email code = do
    renderer <- lLift getUrlRenderParams
    let url = T.pack $ S.renderHtml $ [hamlet|@{PasswordResetR code}|] renderer
    let to = [Address Nothing email]
    let head = [("Subject", "Password reset")]
    let text = TLE.encodeUtf8 [stext|
        You have requested a password reset. Visit the link below to reset it:

        #{url}
    |]
    let textPart = Part { 
        partType = "text/plain; charset=utf-8",
        partEncoding = None,
        partDisposition = DefaultDisposition,
        partContent = PartContent text,
        partHeaders = []
    }
    let html = U.renderHtml [shamlet|
        <p>
            You have requested a password reset. Visit the link below to reset it:
        <p>
            <a href="#{url}">
                #{url}
    |]
    let htmlPart = Part { 
        partType = "text/html; charset=utf-8",
        partEncoding = None,
        partDisposition = DefaultDisposition,
        partContent = PartContent html,
        partHeaders = []
    }
    mail <- initEmptyMail
    sendMail mail
        { mailTo = to, mailHeaders = head, mailParts = [[textPart, htmlPart]] }

invitePasswordReset :: Entity User -> LHandler ()
invitePasswordReset u =
    let (Entity uId user) = u in
    do
    code <- liftIO $ randomString 20
    expires <- do
        now <- getCurrentTime
        -- Add a week.
        return $ addUTCTime (fromInteger 604800) now
    let invitation = PasswordResetInvite uId code expires
    res <- runDB $ insertUnique invitation
    case res of
        Nothing ->
            invitePasswordReset u
        Just _ ->
            sendResetCode (userEmail user) code

deleteResetCode :: Text -> LHandler ()
deleteResetCode code = 
    runDB $ deleteBy $ UniquePasswordResetInvite code

-- Resets a user's password, given a valid reset code, username, and new password. Returns Nothing on success. 
resetPassword :: Text -> Text -> Text -> LHandler (Maybe Text)
resetPassword resetCode account password = do
    res <- runDB [lsql| select User.*, PasswordResetInvite.* from User 
            right outer join PasswordResetInvite on User.id == PasswordResetInvite.account
            where PasswordResetInvite.invite == #{resetCode}
            limit 1
        |]
    -- $ E.select $ E.from $ \( u `E.RightOuterJoin` i) -> do
    --     E.on ( u E.?. UserId E.==. E.just (i E.^. PasswordResetInviteAccount))
    --     E.where_ ( i E.^. PasswordResetInviteInvite E.==. E.val resetCode)
    --     E.limit 1
    --     return ( u, i)
    case res of
        [] ->
            return $ Just "Invalid password reset code."
        [( mUser, (Entity _ invitation))] -> do
            now <- liftIO getCurrentTime
            if now > (passwordResetInviteExpiration invitation) then
                do
                deleteResetCode resetCode
                return $ Just "Reset code has expired."
            else
                case mUser of
                    Nothing ->
                        return $ Just "Account not found."
                    Just (Entity uId user) ->
                        if account /= userIdent user then
                            return $ Just "Invalid username."
                        else
                            do
                            user' <- setPassword password user
                            runDB $ updateWhere [UserId ==. uId] [UserPassword =. userPassword user', UserSalt =. userSalt user']
                            -- $ E.update $ \u -> do
                            --     E.set u [ 
                            --             UserPassword E.=. E.val (userPassword user'),
                            --             UserSalt E.=. E.val (userSalt user')
                            --         ]
                            --     E.where_ ( u E.^. UserId E.==. E.val uId)
                            deleteResetCode resetCode
                            return Nothing
        _ ->
            -- TODO: log runtime error??
            error "User.resetPassword: unreachable"

