module Admin where

import Import
--import Prelude
--import Yesod
--import Core
--import Model
--import Data.Text

data Page = Contests | Users | Teams | Announcements deriving Eq

unless_v :: (Eq a) => a -> b -> b -> (a -> b)
unless_v value vequal vdefault =
      \x -> if (x == value) then vequal else vdefault

setTitle :: Text -> LWidget
setTitle t =
    Import.setTitle [shamlet|#{t} - Admin Tools|]

layoutContest :: Text -> (Entity Contest -> LWidget) -> LHandler Html
layoutContest url content = do
    res <- retrieveContest $ Just url
    layout Contests $ case res of 
        Nothing ->
            contestNotFound
        Just c ->
            content c

layout :: Page -> LWidget -> LHandler Html
layout page content = 
    let unless v = (unless_v v ("active" :: Text) "") page in
    let contestsActive = unless Contests in
    let usersActive = unless Users in
    let teamsActive = unless Teams in
    let announcementsActive = unless Announcements in
    let subtitle = case page of 
          Contests -> "Contests" :: Text
          Users -> "Users"
          Teams -> "Teams"
          Announcements -> "Announcements"
    in do
    raiseUserLabel
    taintLabel $ dcSingleton PrincipalAdmin
    defaultLayout $ do
        content' <- extractWidget content
        [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Admin Tools
                            <small>
                                #{subtitle}
            <div class="row">
                <div class="col-md-3">
                    <ul class="nav nav-pills nav-stacked">
                        <li class="#{contestsActive}">
                            <a href="@{AdminContestsR}">
                                Contests
                        <li class="#{teamsActive}">
                            <a href="@{TodoR}">
                                Teams
                        <li class="#{usersActive}">
                            <a href="@{AdminUsersR}">
                                Users
                        <li class="#{announcementsActive}">
                            <a href="@{AdminAnnouncementsR}">
                                Announcements
                <div class="col-md-9">
                    ^{content'}
        |]

contestNotFound :: LWidget
contestNotFound = do
    Admin.setTitle "Not found"
    [whamlet|
        Contest does not exist.
    |]

userNotFound :: LWidget
userNotFound = do
    Admin.setTitle "Not found"
    [whamlet|
        User does not exist.
    |]
