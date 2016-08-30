module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = redirect ProfileAccountR
