module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR =
    redirect AdminContestsR
