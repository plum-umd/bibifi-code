{-# LANGUAGE FlexibleInstances #-}
module Handler.Profile.Teams where

import Data.Bits (xor)
import Data.Hashable
import qualified Data.HashSet as HashSet
import Import
import qualified Profile

instance Hashable (Entity Team) where
    hashWithSalt salt (Entity tId _) = salt `xor` (fromIntegral $ keyToInt tId)

getProfileTeamsR :: Handler Html
getProfileTeamsR = runLHandler $ Profile.layout Profile.Teams $ \uId -> do
    handlerToWidget $ raiseGroupLabel
    -- select * from team left outer join team_member on team.id = team_member.team where team.leader = uId OR team_member.user = uId;
    teams <- handlerToWidget $ runDB [lsql| select Team.* from Team left outer join TeamMember on Team.id == TeamMember.Team where Team.leader == #{uId} or TeamMember.user == #{Just uId} |]
    -- $ E.selectDistinct $ E.from $ \(t `E.LeftOuterJoin` tm) -> do
    --     E.on (t E.^. TeamId E.==. tm E.^. TeamMemberTeam)
    --     E.where_ ( t E.^. TeamLeader E.==. E.val uId E.||. tm E.^. TeamMemberUser E.==. E.val uId)
    --     return t
    [whamlet|
        <a href="@{CreateTeamR}" type="button" class="btn btn-primary pull-right">
            Create new team
        <div class="clearfix">
    |]
    case teams of
        [] ->
            [whamlet|
                You are not on any teams.
            |]
        _ -> do
            let display t' acc =
                  let (Entity tId t) = t' in
                  let leader = 
                        if (teamLeader t) == uId then
                          [whamlet'|<span class="pull-right label label-primary">Leader</span>|]
                        else
                          mempty
                  in
                  mappend [whamlet'|
                    <a href="@{TeamR tId}" class="list-group-item">
                        #{teamName t}
                        ^{leader}
                  |] acc

            let ts = HashSet.fold display mempty $ HashSet.fromList teams 
            toWidget listGroupStyle
            [whamlet|
                <ul class="list-group vertical-margin">
                    ^{ts}
            |]
