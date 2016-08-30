module Forms.Survey where

import Import
import Forms
import Yesod.Form.Bootstrap3

data SurveyFormData = SurveyFormData {
    -- formLanguage :: Text,
    -- formTimezone :: Text,
    formSchool :: Maybe Text,
    formDegree :: Text,
    formGraduationYear :: Maybe Int, -- TODO
    -- formYearsInProgram :: Int, -- Always -1 now
    formMajor :: Maybe Text,
    formMinor :: Maybe Text,
    formDegreesHeld :: Maybe Text,
    formYearsOfExperience :: Maybe Int,
    formLanguages :: Maybe Text,
    formFavoriteLanguages :: Maybe Text,
    formExperienceClass :: Maybe Bool,
    formExperiencePersonal :: Maybe Bool,
    formExperienceInternship :: Maybe Bool,
--    formExperienceJob :: Bool,
    formYearsOfWork :: Maybe Int,
    formSoftwareEngineering :: Maybe Bool,
    formSecurityClass :: Maybe Bool,
    formSecurityTraining :: Maybe Bool,
    formSecurityExperience :: Maybe Bool,
    formPreviousContest :: Maybe Bool,
    formProgrammerRating :: Maybe Int, -- TODO
    formAttackerRating :: Maybe Int, -- TODO
    formAge :: Maybe Int,
    formGender :: Maybe Text,
    formNationality :: Maybe Text,
    formResumePermission :: Bool,
    formConfirmation :: Bool
    }

-- maybeToText :: Maybe Text -> Text
-- maybeToText t = case t of
--     Nothing ->
--         ""
--     Just t' ->
--         t'

userInformationToSurveyForm :: UserInformation -> SurveyFormData
userInformationToSurveyForm u = SurveyFormData
    -- (userInformationLanguage u)
    -- (userInformationTimezone u)
    (userInformationSchool u)
    (userInformationDegree u)
    (userInformationGraduationYear u)
    (userInformationMajor u)
    (userInformationMinor u)
    (userInformationDegreesHeld u)
    (userInformationYearsOfExperience u)
    (userInformationLanguages u)
    (userInformationFavoriteLanguages u)
    (userInformationExperienceClass u)
    (userInformationExperiencePersonal u)
    (userInformationExperienceInternship u)
    (userInformationYearsOfWork u)
    (userInformationSoftwareEngineering u)
    (userInformationSecurityClass u)
    (userInformationSecurityTraining u)
    (userInformationSecurityExperience u)
    (userInformationPreviousContest u)
    (userInformationProgrammerRating u)
    (userInformationAttackerRating u)
    (userInformationAge u)
    (userInformationGender u)
    (userInformationNationality u)
    (userInformationResumePermission u)
    -- (userInformationConsent u)
    (userInformationAgreeToParticipate u)
            


surveyFormToUserInformation :: UserId -> SurveyFormData -> UserInformation
surveyFormToUserInformation uId dat = UserInformation
    uId
    (formSchool dat)
    (formMajor dat)
    (formMinor dat)
    (formDegreesHeld dat)
    (formDegree dat)
    Nothing -- (formYearsInProgram dat)
    (formYearsOfExperience dat)
    (formLanguages dat)
    (formFavoriteLanguages dat)
    (formYearsOfWork dat)
    (formExperienceClass dat)
    (formExperiencePersonal dat)
    (formExperienceInternship dat)
    Nothing -- (formExperienceJob dat)
    (formSecurityTraining dat)
    (formSecurityExperience dat)
    (formSoftwareEngineering dat)
    (formSecurityClass dat)
    (formPreviousContest dat)
    (formResumePermission dat)
    (formAge dat)
    (formNationality dat)
    (formGender dat)
    (formConfirmation dat)
    (formGraduationYear dat)
    (formProgrammerRating dat)
    (formAttackerRating dat)
    Nothing -- (formLanguage dat)
    Nothing -- (formTimezone dat)

surveyForm :: (Route App -> [(Text,Text)] -> Text) -> Maybe SurveyFormData -> AForm Handler SurveyFormData -- FormRender m SurveyFormData
surveyForm render formM = SurveyFormData
    -- <*> areq languageField (withPlaceholder "In decreasing order of proficiency" (bfs' "Languages spoken")) (f formLanguage)
    -- <*> areq timezoneField (bfs' "Timezone") (f formTimezone)
    <$> aopt universityField (bfs' "University") (f formSchool) -- (Just "University of Maryland, College Park")
    <*> areq (selectFieldList degrees) (bfs' "Degree") (Just $ maybe "Undergraduate" formDegree formM)
    <*> aopt (boundedIntField 1900 2100) (bfs' "Graduation year") (f formGraduationYear)
    -- <*> areq intField (bfs' "Years in program (ex: Freshman is 1)") Nothing
    <*> aopt majorField (bfs' "Major") (f formMajor)
    <*> aopt majorField (bfs' "Minor") (f formMinor) -- (if any)
    <*> aopt degreesField (bfs' "Other degrees") (f formDegreesHeld)
    <*> aopt (boundedIntField 0 150) (bfs' "Years of programming experiences") (f formYearsOfExperience)
    <*> aopt programmingLanguageField (bfs' "What programming languages do you know?") (f formLanguages)
    <*> aopt programmingLanguageField (bfs' "What is your favorite programming language?") (f formFavoriteLanguages)
    <*> aopt boolField' "Have you taken any programming classes?" (f formExperienceClass)
    <*> aopt boolField' "Do you program for any personal projects?" (f formExperiencePersonal)
    <*> aopt boolField' "Have you programmed during an internship?" (f formExperienceInternship)
--    <*> areq boolField' "Have you programmed during a full-time job?" Nothing
    <*> aopt (boundedIntField 0 100) (bfs' "How many years of work experience do you have coding?") (f formYearsOfWork)
    <*> aopt boolField' "Have you taken a software engineering class?" (f formSoftwareEngineering)
    <*> aopt boolField' "Have you taken a computer security class?" (f formSecurityClass)
    <*> aopt boolField' "Have you had any formal training in computer security (aside from this coursera sequence)?" (f formSecurityTraining)
    <*> aopt boolField' "Do you have prior experience in computer security?" (f formSecurityExperience)
    <*> aopt boolField' "Have you participated in other security contests?" (f formPreviousContest)
    <*> aopt rateField (bfs' "How do you rate your abilities as a programmer on a scale from 1 (low) to 10 (high)?") (f formProgrammerRating)
    <*> aopt rateField (bfs' "How do you rate your abilities as an attacker on a scale from 1 (low) to 10 (high)?") (f formAttackerRating)
    <*> aopt (boundedIntField 18 150) (bfs' "Age") (f formAge)
    <*> aopt genderField (bfs' "Gender") (f formGender)
    <*> aopt countryField (bfs' "Nationality") (f formNationality)
    <*> areq boolField' "Would you like us to make your CV available to companies who have sponsored our research, so they can contact you about future employment and internship opportunities?" (formResumePermission <$> formM)
    -- <*> areq boolField' "Would you like to participate in the research study? You will need to read, sign, and submit an agreement to participate in the study. In addition, you will need to upload your CV before the contest begins." (Just True)
    <*> areq (checkBoxField' $ [hamlet|I agree to participate in the research study and the terms in this <a href="@{StaticR doc_consent_form_pdf}" target="_blank">consent form</a>|] render) "We ask that you allow us to use data gathered from your performance in this contest as part of a research study that aims to better understand how to build secure systems. Your identity will be held in strict confidence (unless you opt to share your information)." (Just $ maybe True formResumePermission formM)
    -- <*> areq (checkBoxField' "I agree") "I understand that I will need to read, sign, and submit an agreement to participate in this contest. In addition, I will upload my CV before the contest begins." Nothing
    where
        degrees = [("Undergraduate"::Text, "Undergraduate"), ("Masters","Masters"),("Doctorate","Doctorate"), ("High school","High school"),("None","None")]

        f :: (SurveyFormData -> Maybe a) -> Maybe (Maybe a)
        f g = fmap (g) formM


