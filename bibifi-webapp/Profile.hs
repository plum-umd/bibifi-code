module Profile where

import Import

data Page = Account | Teams

layout :: Page -> (UserId -> LWidget) -> LHandler Html
layout page content =
    let accountActive = case page of
          Account -> "active" :: Text
          _ -> ""
    in
    let teamsActive = case page of
          Teams -> "active" :: Text
          _ -> ""
    in
    let subtitle = case page of
          Account -> "Account" :: Text
          Teams -> "Teams"
    in
    do
    uId <- requireAuthId
    defaultLayout $ do
        setTitle [shamlet|#{subtitle} - Profile|]
        content' <- extractWidget $ content uId
        [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Profile
                            <small>
                                #{subtitle}
            <div class="row">
                <div class="col-md-3">
                    <ul class="nav nav-pills nav-stacked">
                        <li class="#{accountActive}">
                            <a href="@{ProfileAccountR}">
                                Account
                        <li class="#{teamsActive}">
                            <a href="@{ProfileTeamsR}">
                                Teams
                <div class="col-md-9">
                    ^{content'}
        |]


