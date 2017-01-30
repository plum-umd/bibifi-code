module Handler.Admin.Teams where

import qualified Admin
import Import

getAdminTeamsR :: Handler Html
getAdminTeamsR = runLHandler $ Admin.layout Admin.Teams $ do
    Admin.setTitle "Teams"
    teams <- handlerToWidget $ runDB $ selectList [] [Asc TeamName]
    case teams of 
        [] ->
            [whamlet|
                <p>
                    There are no teams.
            |]
        _ -> do
            let ts = mconcat $ map display teams
            toWidget listGroupStyle
            [whamlet|
                <ul .list-group .vertical-margin>
                    ^{ts}
            |]

    where
        display (Entity tId t) = 
            [whamlet'|
                <a href="@{AdminTeamR tId}" .list-group-item>
                    #{teamName t}
            |]
