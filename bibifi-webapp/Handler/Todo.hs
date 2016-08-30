module Handler.Todo where

import Import

getTodoR :: Handler Html
getTodoR = runLHandler $ defaultLayout [whamlet|
This page is currently under construction. 
|]
