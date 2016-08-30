module Handler.Admin.Announcements (getAdminAnnouncementsR)
--       ,postAddAnnouncementR)
where

import Import
import qualified Admin

data FormData = FormData {
  formTitle :: Text
}

getAdminAnnouncementsR :: Handler Html
getAdminAnnouncementsR = runLHandler $ Admin.layout Admin.Announcements $ do
    Admin.setTitle "Announcements"
    res <- handlerToWidget $ runDB [lsql|
            select Contest.title, Post.* from Contest inner join Post 
            on Contest.id == Post.contest 
            order by Post.timestamp desc 
        |]
        -- $ E.select $ E.from $ \(c `E.InnerJoin` a) -> do
        -- E.on ( c E.^. ContestId E.==. a E.^. PostContest)
        -- E.orderBy [ E.desc (a E.^. PostTimestamp)]
        -- return ( c E.^. ContestTitle, a)
    [whamlet|
        <a href="@{AddAnnouncementR}" type="button" class="btn btn-primary pull-right">
            Create new announcement
        <div class="clearfix">
    |]
    case res of
        [] ->
            [whamlet|
                There are no announcements.
            |]
        announcements -> do
            let display ( title, Entity id post) =
                  let isdraft = do
                        if postDraft post then
                            [whamlet'|<span class="pull-right label label-primary">Draft</span>|]
                        else
                            mempty
                  in
                  [whamlet'|
                    <a href="@{ModifyAnnouncementR id}" class="list-group-item">
                       #{postTitle post}
                       <small .text-muted>
                           #{title}
                       ^{isdraft}
                       

                  |]
            let cs = mconcat $ map display announcements
            toWidget listGroupStyle
            [whamlet|
                <ul class="list-group vertical-margin">
                    ^{cs}
            |]

