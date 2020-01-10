{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module PostDependencyType where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.TH
import Prelude
import Yesod

data PostDependencyType = Round1Start | Round1End | Round2Start | Round2End | Round3Start | Round3End
    deriving (Show, Read, Eq)
derivePersistField "PostDependencyType"
Aeson.deriveJSON Aeson.defaultOptions ''PostDependencyType

data ContestRound = ContestRoundBuild | ContestRoundBreak | ContestRoundFix
    deriving (Show, Read, Eq)
derivePersistField "ContestRound"
Aeson.deriveJSON Aeson.defaultOptions ''ContestRound

data OracleSubmissionStatus = OraclePending | OracleRunning | OracleFinished | OracleError
    deriving (Show, Read, Eq)
derivePersistField "OracleSubmissionStatus"
Aeson.deriveJSON Aeson.defaultOptions ''OracleSubmissionStatus

prettyOracleStatus :: OracleSubmissionStatus -> Html
prettyOracleStatus OraclePending = [shamlet|
        <span>
            Pending
    |]
prettyOracleStatus OracleRunning = [shamlet|
        <span>
            Running
    |]
prettyOracleStatus OracleFinished = [shamlet|
        <span class="text-success">
            Finished
    |]
prettyOracleStatus OracleError = [shamlet|
        <span class="text-danger">
            Error
    |]

data BuildSubmissionStatus = BuildPullFail | BuildPending | BuildBuilding | BuildBuildFail | BuildBuilt | BuildTimeout
    deriving (Show, Read, Eq)
derivePersistField "BuildSubmissionStatus"
Aeson.deriveJSON Aeson.defaultOptions ''BuildSubmissionStatus

data BreakSubmissionStatus = BreakPullFail | BreakPending | BreakTesting | 
    BreakTested | BreakRejected | BreakTimeout
    -- BreakJudging | BreakJudged
    deriving (Show, Read, Eq)
derivePersistField "BreakSubmissionStatus"
Aeson.deriveJSON Aeson.defaultOptions ''BreakSubmissionStatus

data BreakSubmissionResult = 
      BreakSucceeded
    | BreakFailed
        deriving (Show, Read, Eq)
derivePersistField "BreakSubmissionResult"
Aeson.deriveJSON Aeson.defaultOptions ''BreakSubmissionResult

data BreakType = 
      BreakCorrectness
    | BreakCrash
    | BreakConfidentiality
    | BreakIntegrity
    | BreakAvailability
    | BreakSecurity
        deriving (Show, Read, Eq)
derivePersistField "BreakType"
-- Aeson.deriveJSON Aeson.defaultOptions ''BreakType

instance ToJSON BreakType where
    toJSON BreakCorrectness = "correctness"
    toJSON BreakCrash = "crash"
    toJSON BreakConfidentiality = "confidentiality"
    toJSON BreakIntegrity = "integrity"
    toJSON BreakAvailability = "availability"
    toJSON BreakSecurity = "security"

instance FromJSON BreakType where
    parseJSON (Aeson.String t) = case Text.toLower t of
        "correctness" -> return BreakCorrectness
        "crash" -> return BreakCrash
        "confidentiality" -> return BreakConfidentiality
        "integrity" -> return BreakIntegrity
        "availability" -> return BreakAvailability
        "security" -> return BreakSecurity
        _ -> fail "Invalid BreakType"
    parseJSON _ = fail "Invalid BreakType"

data FixSubmissionStatus = FixPending | FixBuilding | FixBuilt | FixJudging | FixJudged | FixTimeout | FixRejected
    -- | FixBuildFail | FixPullFail | FixInvalidBugId -- Deprecated.
    deriving (Show, Read, Eq)
derivePersistField "FixSubmissionStatus"
Aeson.deriveJSON Aeson.defaultOptions ''FixSubmissionStatus

data FixSubmissionResult = FixFixed | FixNotFixed 
    -- | FixDisqualified -- TODO: Set the message as disqualified instead.
    deriving (Show, Read, Eq)
derivePersistField "FixSubmissionResult"
Aeson.deriveJSON Aeson.defaultOptions ''FixSubmissionResult

prettyBuildStatus :: BuildSubmissionStatus -> Html
prettyBuildStatus s = case s of 
    BuildPullFail -> 
        [shamlet|
            <span class="text-danger">
                Pull failed
        |]
    BuildPending -> 
        [shamlet|
            <span>
                Pending
        |]
    BuildBuilding -> 
        [shamlet|
            <span>
                Building
        |]
    BuildTimeout ->
        [shamlet|
            <span class="text-danger">
                Submission timed out
        |]
    BuildBuildFail ->
        [shamlet|
            <span class="text-danger">
                Build failed
        |]
    BuildBuilt ->
        [shamlet|
            <span class="text-success">
                Built
        |]

prettyPassResult :: Bool -> Html
prettyPassResult pass = 
    if pass then
        [shamlet|
            <span class="text-success">
                Pass
        |]
    else
        [shamlet|
            <span class="text-danger">
                Fail
        |]

prettyBreakStatus :: BreakSubmissionStatus -> Html
prettyBreakStatus s = case s of
    BreakPullFail ->
        [shamlet|
            <span class="text-danger">
                Pull failed
        |]
    BreakPending ->
        [shamlet|
            <span>
                Pending
        |]
    BreakTesting ->
        [shamlet|
            <span>
                Testing
        |]
    BreakTested ->
        [shamlet|
            <span class="text-success">
                Tested
        |]
    BreakTimeout ->
        [shamlet|
            <span class="text-danger">
                Submission timed out
        |]
    BreakRejected ->
        [shamlet|
            <span class="text-danger">
                Rejected
        |]
    -- BreakJudging ->
    --     [shamlet|
    --         <span>
    --             Judging required
    --     |]
    -- BreakJudged ->
    --     [shamlet|
    --         <span>
    --             Judged
    --     |]

prettyBreakStatusVictim :: BreakSubmissionStatus -> Html
prettyBreakStatusVictim s = 
    let txt = case s of
          BreakPullFail ->
            "Pull failed" :: Text
          BreakPending ->
            "Pending"
          BreakTesting ->
            "Testing"
          BreakTested ->
            "Tested"
          BreakTimeout ->
            "Submission timed out"
          BreakRejected ->
            "Rejected"
          -- BreakJudging ->
          --   "Judging required"
          -- BreakJudged ->
          --   "Judged"
    in
    [shamlet|
        <span>
            #{txt}
    |]
        
prettyBreakResult :: Maybe BreakSubmissionResult -> Html
prettyBreakResult s = case s of
    Nothing ->
        [shamlet|
            <span>
                &#8212;
        |]
    Just BreakSucceeded ->
        [shamlet|
            <span class="text-success">
                Break succeeded
        |]
    Just BreakFailed ->
        [shamlet|
            <span class="text-danger">
                Break failed
        |]
        
prettyBreakResultVictim :: Maybe BreakSubmissionResult -> Html
prettyBreakResultVictim s = case s of
    Nothing ->
        [shamlet|
            <span>
                &#8212;
        |]
    Just BreakSucceeded ->
        [shamlet|
            <span class="text-danger">
                Break succeeded
        |]
    Just BreakFailed ->
        [shamlet|
            <span class="text-danger">
                Break failed
        |]
        
prettyFixStatus :: FixSubmissionStatus -> Html
prettyFixStatus s = case s of 
    FixPending ->
        [shamlet|
            <span>
                Pending
        |]
    FixBuilding ->
        [shamlet|
            <span>
                Building
        |]
    FixTimeout ->
        [shamlet|
            <span class="text-danger">
                Submission timed out
        |]
    FixRejected ->
        [shamlet|
            <span class="text-danger">
                Fix rejected
        |]
    FixBuilt ->
        [shamlet|
            <span class="text-success">
                Built
        |]
    FixJudging ->
        [shamlet|
            <span>
                Judgement Required
        |]
    FixJudged ->
        [shamlet|
            <span>
                Judged
        |]

prettyFixResult :: Maybe FixSubmissionResult -> Html
prettyFixResult r = case r of
    Nothing ->
        [shamlet|
            <span>
                &#8212;
        |]
    Just FixFixed ->
        [shamlet|
            <span class="text-success">
                Fixed
        |]
    Just FixNotFixed ->
        [shamlet|
            <span class="text-danger">
                Not fixed
        |]
--     Just FixDisqualified ->
--         [shamlet|
--             <span class="text-danger">
--                 Disqualified
--         |]

prettyBreakType :: BreakType -> Html
prettyBreakType BreakCorrectness = [shamlet|
        <span>
            Correctness
    |]
prettyBreakType BreakCrash = [shamlet|
        <span>
            Crash
    |]
prettyBreakType BreakIntegrity = [shamlet|
        <span>
            Integrity
    |]
prettyBreakType BreakAvailability = [shamlet|
        <span>
            Availability
    |]
prettyBreakType BreakConfidentiality = [shamlet|
        <span>
            Confidentiality
    |]
prettyBreakType BreakSecurity = [shamlet|
        <span>
            Security
    |]
