module Handler.Admin.Announcement (getAdminContestR) where

import Import
import qualified Admin

-- TODO: UNUSED XXX
-- getAdminContestR :: Text -> Handler Html
-- getAdminContestR url = do
--     res <- retrieveContest $ Just url
--     Admin.layout Admin.Contests $ do
--         case res of 
--             Nothing ->
--                 Admin.contestNotFound
--             Just (Entity _ c) -> do
--                 Admin.setTitle $ contestTitle c
--                 [whamlet|
--                     <h2>
--                         #{contestTitle c}
--                     <h3>
--                         Announcements
--                     <p>
--                         <a href="@{TodoR}">
--                             Create new announcements, edit them, delete them, etc. 
--                     <h3>
--                         Participants
--                     <p>
--                         <a href="@{TodoR}">
--                             View participants, inspect their submissions, etc.
--                     <h3>
--                         Default
--                     <p>
--                         <a href="@{MakeDefaultR url}">
--                             Make this contest the default contest.
--                 |]
    
