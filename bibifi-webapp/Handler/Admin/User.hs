module Handler.Admin.User where

import Import
import qualified Admin

getAdminUserR :: UserId -> Handler Html
getAdminUserR uId = runLHandler $ Admin.layout Admin.Users $ do
    res <- handlerToWidget $ runDB [lsql|
            select * from User
            inner join UserInformation on User.id == UserInformation.user
            where User.id == #{uId}
            limit 1
        |]
    -- $ E.select $ E.from $ \(u `E.InnerJoin` i) -> do
    --     E.on (u E.^. UserId E.==. i E.^. UserInformationUser)
    --     E.where_ (u E.^. UserId E.==. E.val uId)
    --     E.limit 1
    --     return ( u, i)
    case res of
        [] ->
            Admin.userNotFound
        [(Entity _ user, Entity _ info)] -> do
            Admin.setTitle "User Information"
            [whamlet|
                <h2>
                    User Information
                <h3>
                    Reset Password
                <a href="@{AdminUserResetPasswordR uId}">
                    Reset user's password
            |]
    
