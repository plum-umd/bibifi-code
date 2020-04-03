module Participation where

import Import
import qualified Team

data Page = Information | BuildSubmissions | BuilderCode | BreakSubmissions | FixSubmissions | Oracle

checkCreds :: TeamContestId -> LHandler (UserId, TeamContest, Contest, Team)
checkCreds tcId = do
    uId  <- requireAuthId
    -- Get TeamContest
    tc' <- runDB $ get tcId
    case tc' of
        Nothing -> 
            notFound
        Just tc -> do
            contest' <- runDB $ get $ teamContestContest tc
            case contest' of
                Nothing ->
                    notFound
                Just contest -> do
                    -- Get the team. This implicitly checks that user is on the team. 
                    team <- Team.getTeam (teamContestTeam tc)
                    return ( uId, tc, contest, team)

layout :: Page -> TeamContestId -> (UserId -> TeamContest -> Contest -> Team -> LWidget) -> LHandler Html
layout page tcId content = 
    let informationActive = case page of
          Information -> "active" :: Text
          _ -> ""
    in
    let oracleActive = case page of
          Oracle -> "active" :: Text
          _ -> ""
    in
    let submissionsActive = case page of 
          BuildSubmissions -> "active" :: Text
          _ -> ""
    in
    let builderCodeActive = case page of
          BuilderCode -> "active" :: Text
          _ -> ""
    in
    let breakActive = case page of 
          BreakSubmissions -> "active" :: Text
          _ -> ""
    in
    let fixActive = case page of
          FixSubmissions -> "active" :: Text
          _ -> ""
    in
    let subtitle = case page of
          Information -> "Information" :: Text
          Oracle -> "Oracle Submissions"
          BuildSubmissions -> "Build Submissions"
          BuilderCode -> "Builders' Code"
          BreakSubmissions -> "Break Submissions"
          FixSubmissions -> "Fix Submissions"
    in
    do
    raiseTeamLabel
    raiseGroupLabel
    ( uId, tc, contest, team) <- checkCreds tcId
    now <- getCurrentTime
    let builderCodeWidget = if now > contestBreakFixStart contest then
            [whamlet'|
                <li class="#{builderCodeActive}">
                    <a href="@{ParticipationBuildersCodeR tcId}">
                        Builders' Code
            |]
          else
            mempty
    let oracleWidget = if development || now > contestBuildStart contest then
            [whamlet'|
                <li class="#{oracleActive}">
                    <a href="@{ParticipationOracleSubmissionsR tcId}">
                        Oracle Submissions
            |]
          else
            mempty
    defaultLayout $ do
        setTitle [shamlet|#{subtitle} - Team Participation|]
        let content' = content uId tc contest team
        [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Team Participation
                            <small>
                                #{subtitle}
            <div class="row">
                <div class="col-md-3">
                    <ul class="nav nav-pills nav-stacked">
                        <li class="#{informationActive}">
                            <a href="@{ParticipationInformationR tcId}">
                                Information
                        ^{oracleWidget}
                        <li class="#{submissionsActive}">
                            <a href="@{ParticipationBuildSubmissionsR tcId}">
                                Build Submissions
                        ^{builderCodeWidget}
                        <li class="#{breakActive}">
                            <a href="@{ParticipationBreakSubmissionsR tcId}">
                                Break Submissions
                        <li class="#{fixActive}">
                            <a href="@{ParticipationFixSubmissionsR tcId}">
                                Fix Submissions
                <div class="col-md-9">
                    ^{content'}
        |]
