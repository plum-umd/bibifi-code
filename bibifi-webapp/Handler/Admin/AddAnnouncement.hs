module Handler.Admin.AddAnnouncement (postModifyAnnouncementR, getModifyAnnouncementR, postDeleteAnnouncementR, postAddAnnouncementR, getAddAnnouncementR)
where 

import Data.Maybe
import Data.Tuple
import Data.Text (pack)
import qualified Data.Text.Lazy as LazyT
import Control.Applicative
import Control.Arrow
import qualified Text.Markdown as M
import Yesod.Auth
import Yesod.Form.Bootstrap3
import Import hiding (renderBootstrap3)
import Forms
import User

data FormData = FormData {
    formTitle :: Text,
    formContest :: ContestId,
    formMarkdown :: Textarea,
    formDraft :: Bool
    }

modifyPostForm :: [Entity Contest] -> Maybe Post -> Form FormData 
modifyPostForm contests input = do
 let contests' = map ((contestTitle . entityVal) &&& entityKey) contests
 renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 2) (ColMd 0) (ColMd 8)) $ FormData
    <$> areq textField (bfs' "Title") (maybe Nothing (Just . postTitle) input)
    <*> areq (selectFieldList contests') (bfs' "Contest") (maybe Nothing (Just . postContest) input)
    <*> areq textareaField (bfs' "Announcement") (maybe Nothing (Just . Textarea . postMarkdown) input)
    <*> areq checkBoxField (bfs' "Draft") (maybe Nothing (Just . postDraft) input)
    <*  bootstrapSubmit (BootstrapSubmit (submittxt) "btn-primary" [])
    where
      submittxt = if isNothing input then ("Add"::Text) else "Modify"

data DeleteForm = DeleteForm PostId

deletePostForm :: Maybe PostId -> Form DeleteForm
deletePostForm post = do
  renderBootstrap3 (BootstrapInlineForm) $ DeleteForm
    <$> areq hiddenField (bfs' "Post ID") (post)
    <* bootstrapSubmit (BootstrapSubmit ("Delete Post"::Text) "btn btn-danger pull-right" [])

generateEditPostHtml :: PostId -> Widget -> Enctype -> Widget -> Enctype -> [Text] -> LHandler Html
generateEditPostHtml postid editwidget editenctype delwidget delenctype msg = 
    let title = generatePageTitle Nothing "Edit Announcement" in
    defaultLayout $
        let msgH = mconcat $ map displayError msg in
        do
        setTitle $ toHtml title
        [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Edit Announcement
                    ^{msgH}
                    <form .form-inline role=form method=post action="@{DeleteAnnouncementR}" enctype=#{delenctype}>
                        ^{delwidget}
                    <form method=post action="@{ModifyAnnouncementR postid}" enctype=#{editenctype} class="form-horizontal" role="form">
                        ^{editwidget}
        |]

generateAddPostHtml :: Widget -> Enctype -> [Text] -> LHandler Html
generateAddPostHtml editwidget editenctype msg = 
    let title = generatePageTitle Nothing "Add Announcement" in
    defaultLayout $
        let msgH = mconcat $ map displayError msg in
        do
        setTitle $ toHtml title
        [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Add Announcement
                    ^{msgH}
                    <form method=post action="@{AddAnnouncementR}" enctype=#{editenctype} class="form-horizontal" role="form">
                        ^{editwidget}
        |]

getAddAnnouncementR :: Handler Html
getAddAnnouncementR = runLHandler $ do
  contests <- runDB $ selectList [] [Desc ContestBuildStart]
  (widget, enctype) <- generateFormPost $ modifyPostForm contests Nothing
  generateAddPostHtml widget enctype []

generatePost :: Text -> ContestId -> Bool -> Textarea -> LHandler Post
generatePost title contest draft markdowntext = do
  time <- getCurrentTime
  let markdown = unTextarea markdowntext
  let html = M.markdown M.def $ LazyT.fromStrict markdown
  return $ Post
           title
           contest
           time
           draft
           html
           markdown
  
addAnnouncement :: Text -> ContestId -> Bool -> Textarea -> LHandler ()
addAnnouncement title contest draft markdown = do
  announcement <- generatePost title contest draft markdown
  runDB $ insert_ announcement

modifyAnnouncement :: PostId -> Text -> ContestId -> Bool -> Textarea -> LHandler ()
modifyAnnouncement post title contest draft markdown = do
  newpost <- generatePost title contest draft markdown
  runDB $ replace post newpost

postAddAnnouncementR :: Handler Html
postAddAnnouncementR = runLHandler $ do
  raiseUserLabel
  contests <- runDB $ selectList [] [Desc ContestBuildStart]
  ((res, widget), enctype) <- runFormPost $ modifyPostForm contests Nothing
  case res of
    FormFailure _msg ->
      generateAddPostHtml widget enctype [] -- msg
    FormMissing ->
      generateAddPostHtml widget enctype []
    FormSuccess (FormData title contest markdown draft) -> do
      addAnnouncement title contest draft markdown
      setMessage [shamlet|
                  <div class="container">
                     <div class="alert alert-success">
                        Announcement added!
                  |]
      redirect $ AdminAnnouncementsR


postModifyAnnouncementR :: PostId -> Handler Html
postModifyAnnouncementR postid = runLHandler $ do
  raiseUserLabel
  res <- runDB $ get postid
  case res of
    Nothing -> do
      redirect $ AdminAnnouncementsR
    Just _ -> do
      contests <- runDB $ selectList [] [Desc ContestBuildStart]
      ((res, widget), enctype) <- runFormPost $ modifyPostForm contests Nothing
      case res of
        FormFailure _msg ->
          generateAddPostHtml widget enctype [] -- msg
        FormMissing ->
          generateAddPostHtml widget enctype []
        FormSuccess (FormData title contest markdown draft) -> do
          modifyAnnouncement postid title contest draft markdown
          setMessage [shamlet|
                       <div class="container">
                         <div class="alert alert-success">
                           Announcement modified!
                     |]
          redirect $ AdminAnnouncementsR

getModifyAnnouncementR :: PostId -> Handler Html
getModifyAnnouncementR postid = runLHandler $ do
  raiseUserLabel
  contests <- runDB $ selectList [] [Desc ContestBuildStart]
  res <- runDB $ get postid
  case res of
    Nothing -> do
      -- Error
      redirect $ AdminAnnouncementsR
    Just post -> do
      let form = FormData (postTitle post) (postContest post) (Textarea $ postMarkdown post) (postDraft post)
      (dwidget, denctype) <- generateFormPost $ deletePostForm $ Just postid
      (widget, enctype) <- generateFormPost $ modifyPostForm contests $ Just post
      generateEditPostHtml postid widget enctype dwidget denctype []

postDeleteAnnouncementR :: Handler Html
postDeleteAnnouncementR = runLHandler $ do
  raiseUserLabel
  ((res, widget), enctype) <- runFormPost $ deletePostForm Nothing
  case res of
    FormFailure _msg ->
      -- Error
      redirect $ AdminAnnouncementsR
    FormMissing ->
      redirect $ AdminAnnouncementsR
    FormSuccess (DeleteForm postid) -> do
      runDB $ delete $ postid
      setMessage [shamlet|
                  <div class="container">
                     <div class="alert alert-success">
                        Announcement deleted
                  |]
      redirect $ AdminAnnouncementsR
