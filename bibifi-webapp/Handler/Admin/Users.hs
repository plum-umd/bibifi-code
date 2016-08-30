module Handler.Admin.Users where

import Import
import qualified Admin

getAdminUsersR :: Handler Html
getAdminUsersR = runLHandler $ Admin.layout Admin.Users $ do
    Admin.setTitle "Users"
    res <- handlerToWidget $ runDB $ selectList [] [Asc UserIdent]
    case res of 
        [] ->
            [whamlet|
                There are no users.
            |]
        users ->
            let display (Entity uId u) = 
                  let admin = 
                        if userAdmin u then
                            [whamlet'|<span class="pull-right label label-primary">Admin</span>|]
                        else
                            mempty
                  in
                  [whamlet'|
                      <a href="@{AdminUserR uId}" .list-group-item>
                          #{userIdent u}
                          ^{admin}
                  |]
            in
            let us = mconcat $ map display users in
            do
            toWidget listGroupStyle
            [whamlet|
                <ul class="list-group vertical-margin">
                    ^{us}
            |]

