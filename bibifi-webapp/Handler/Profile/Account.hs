{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Profile.Account where

import Import
import Data.Text (pack)
import qualified Profile

getProfileAccountR :: Handler Html
getProfileAccountR = runLHandler $
    Profile.layout Profile.Account $ \uId -> do
        queryres <- handlerToWidget $ runDB [lsql|
                select User.*, UserInformation.* from User
                left outer join UserInformation on User.id == UserInformation.user
                where User.id == #{uId}
                limit 1
            |]
            --      $ runDB $ E.select 
            --              $ E.from $ \(u `E.LeftOuterJoin` ui) -> do
            --                E.on (E.just (u E.^. UserId) E.==. ui E.?. UserInformationUser)
            --                E.where_ (u E.^. UserId E.==. E.val uId)
            --                E.limit 1
            --                return (u,ui)
        case queryres of 
            [((Entity _ user), mInfo)] -> do
                let flds' = case mInfo of
                      Just (Entity _ information) ->
                        let flds = [("School"::Text,na . userInformationSchool)
                                   ,("Graduation year", intText. userInformationGraduationYear)
                                   ,("Major",na . userInformationMajor)
                                   ,("Minor",na . userInformationMinor)
                                   ,("Degrees held",na . userInformationDegreesHeld)
                                   ,("Degree",na . Just . userInformationDegree)
                                   -- ,("Years in program"
                                   --  ,(pack . show . userInformationYearsInProgram))
                                   ,("Years of experience"
                                    ,(intText . userInformationYearsOfExperience))
                                   ,("Languages known",na . userInformationLanguages)
                                   ,("Favorite languages",na . userInformationFavoriteLanguages)
                                   ,("Class experience?"
                                    ,(yesno . userInformationExperienceClass))
                                   ,("Personal experience?"
                                    ,(yesno . userInformationExperiencePersonal))
                                   ,("Internship experience?"
                                    ,(yesno . userInformationExperienceInternship))
--                                    ,("Job experience?"
--                                     ,(yesno . userInformationExperienceJob))
                                   ,("Security training?"
                                    ,(yesno . userInformationSecurityTraining))
                                   ,("Security experience?"
                                    ,(yesno . userInformationSecurityExperience))
                                   ,("Soft eng. experience?"
                                    ,(yesno . userInformationSoftwareEngineering))
                                   ,("Taken security class?"
                                    ,(yesno . userInformationSecurityClass))
                                   ,("Previous contestant?"
                                    ,(yesno . userInformationPreviousContest))
                                   , ("Programmer rating", ratingText userInformationProgrammerRating)
                                   , ("Attacker rating", ratingText userInformationAttackerRating)
                                   ,("Age",(intText . userInformationAge))
                                   ,("Gender", na . userInformationGender)
                                   ,("Nationality", na . userInformationNationality)
                                   ]
                        in
                        let lblhtml (fldtxt,anstext) = [whamlet|
                                <label class="col-md-4 control-label">
                                   #{fldtxt}
                                <div class="col-md-8">
                                   <p class="form-control-static">
                                     #{anstext}
                            |] :: LWidget
                        in
                        mconcat $ map (lblhtml . (\(txt,f) -> (txt,f information))) flds
                      _ ->
                        mempty
                created <- liftIO $ displayTime $ userCreated user
                let consentField = case userConsentForm user of
                      Nothing -> [whamlet'|
                            Not uploaded (<a href="@{ProfileAccountConsentR}">Upload here</a>)
                        |]
                      Just fileId -> [whamlet'|
                            <a href="@{DownloadFileR fileId}">
                                Uploaded 
                            (<a href="@{ProfileAccountConsentR}">Upload again</a>)
                        |]
                let resumeField = case userResume user of
                      Nothing -> [whamlet'|
                            Not uploaded (<a href="@{ProfileAccountResumeR}">Upload here</a>)
                        |]
                      Just fileId -> [whamlet'|
                            <a href="@{DownloadFileR fileId}">
                                Uploaded
                           (<a href="@{ProfileAccountResumeR}">Upload again</a>)
                        |]
                pendingInvites <- do
                    invites <- handlerToWidget $ runDB $ selectList [TeamInviteEmail ==. userEmail user] []
                    case invites of
                        [] ->
                            return mempty
                        _ -> 
                            let renderInvites (Entity _ i) = [whamlet'|
                                    <p>
                                        <a href="@{TeamInvitationR (teamInviteInvite i)}">
                                            @{TeamInvitationR (teamInviteInvite i)}
                                  |]
                            in
                            let ws = mconcat $ map renderInvites invites in
                            return [whamlet'|
                                <h2>
                                    Pending team invitations
                                ^{ws}
                            |]
                [whamlet|
                    <a type=button .pull-right .btn .btn-default href="@{ProfileAccountEditR}">
                        Edit
                    <div .clear>
                    <form class="form-horizontal">
                        <div class="form-group">
                            <label class="col-md-4 control-label">
                                Username
                            <div class="col-md-8">
                                <p class="form-control-static">
                                    #{userIdent user}
                            <label class="col-md-4 control-label">
                                Email
                            <div class="col-md-8">
                                <p class="form-control-static">
                                    #{userEmail user}
                            <label class="col-md-4 control-label">
                                Created
                            <div class="col-md-8">
                                <p class="form-control-static">
                                    #{created}

                            <label class="col-md-4 control-label">
                                Consent form
                            <div class="col-md-8">
                                <p class="form-control-static">
                                    ^{consentField}

                            <label class="col-md-4 control-label">
                                Resume
                            <div class="col-md-8">
                                <p class="form-control-static">
                                    ^{resumeField}

                            ^{flds'}
                    ^{pendingInvites}
                |] :: LWidget
            _ -> do
                -- TODO: log this
                setMessage [shamlet|
                    <div class="container">
                        <div class="alert alert-danger">
                            Error: Unable to find user.
                |]
                redirect AnnouncementsR

    where
        intText (Just i) = pack (show i)
        intText Nothing = "N/A"

        ratingText f r = case f r of
            Nothing -> "N/A"
            Just i -> pack (show i) <> "/10"

        na Nothing = "N/A" :: Text
        na (Just "") = "N/A"
        na (Just t) = t

        yesno (Just True) = "Yes" :: Text
        yesno (Just False) = "No"
        yesno Nothing = "N/A"
