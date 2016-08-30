module Handler.Team.AddMembers where

import Import

import Forms
import qualified Team

data PostData = PostData [Text]

teamForm :: Form PostData
teamForm = renderBootstrap3' $ PostData
    <$> areq listEmailField "Team members" Nothing

generateWidget :: TeamId -> Widget -> Enctype -> [Text] -> LWidget
generateWidget tId widget enctype msg = do
    let msgH = mconcat $ map displayError msg
    [whamlet|
        ^{msgH}
        <form method=post action=@{TeamAddMembersR tId} enctype=#{enctype}>
            ^{widget}
            <div class="form-group">
                <div class="col-sm-offset-2 col-sm-10">
                    <button type="submit" class="btn btn-default">
                        Submit
    |]


getTeamAddMembersR :: TeamId -> Handler Html
getTeamAddMembersR tId = runLHandler $ Team.layout Team.AddMembers tId $ \uId (Team teamName leaderId) ->
    if uId /= leaderId then
        [whamlet|Only team leaders can add team members.|]
    else
        do
        ( widget, enctype) <- handlerToWidget $ generateFormPost teamForm
        generateWidget tId widget enctype []


postTeamAddMembersR :: TeamId -> Handler Html
postTeamAddMembersR tId = runLHandler $ Team.layout Team.AddMembers tId $ \uId (Team teamName leaderId) ->
    if uId /= leaderId then
        redirect $ TeamAddMembersR tId
    else
        do
        ((res, widget), enctype) <- handlerToWidget $ runFormPost teamForm
        case res of
            FormSuccess (PostData emails) -> do
                handlerToWidget $ do
                    Team.inviteTeam tId emails
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Invitations have been emailed to your team members!
                    |]
                redirect $ TeamAddMembersR tId
            FormFailure _msg ->
                generateWidget tId widget enctype [] -- msg
            FormMissing ->
                generateWidget tId widget enctype []
    

-- TODO: check permissions!!!
