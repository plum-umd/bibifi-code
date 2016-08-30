module Handler.Team where

import Import

-- teamForm :: Form 
-- 
-- generateHtml :: TeamId -> UserId -> Widget -> Enctype -> [Text] -> Handler Html
-- generateHtml tId uId widget enctype msg = do
--     teams <- runDB $ E.select $ E.from $ \(t `E.LeftOuterJoin` tm) -> do
--         E.on (t E.^. TeamId E.==. tm E.^. TeamMemberTeam)
--         E.where_ ( t E.^. TeamId E.==. tId E.&&. 
--             (t E.^. TeamLeader E.==. E.val uId 
--             E.||. tm E.^. TeamMemberUser E.==. E.val uId))
--         E.limit 1
--         return t
--     -- Check that user is on team.
--     case teams of
--         [team] ->
--             defaultLayout
--                 [whamlet|TODO|]
--         _ ->
--             setMessage [shamlet|
--                 <div class="container">
--                     <div class="alert alert-danger">
--                         Team not found.
--             |]
--             redirect AnnouncementsR

getTeamR :: TeamId -> Handler Html
getTeamR tId = redirect (TeamInformationR tId)

    -- TODO: redirect to TeamInformationR
--    defaultLayout   
--        [whamlet|TODO|]
--     ( widget, enctype) <- generateFormPost teamForm
--     generateHtml tId uId widget enctype []
-- 
-- postTeamR :: TeamId -> Handler Html
-- postTeamR tId = defaultLayout
--     [whamlet|TODO|]
