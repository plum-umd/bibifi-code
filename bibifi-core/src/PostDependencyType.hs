{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module PostDependencyType where

import Data.Text (Text)
import Database.Persist.TH
import Prelude
import Yesod

data PostDependencyType = Round1Start | Round1End | Round2Start | Round2End | Round3Start | Round3End
    deriving (Show, Read, Eq)
derivePersistField "PostDependencyType"

data ContestRound = ContestRoundBuild | ContestRoundBreak | ContestRoundFix
    deriving (Show, Read, Eq)
derivePersistField "ContestRound"

data OracleSubmissionStatus = OraclePending | OracleRunning | OracleFinished | OracleError
    deriving (Show, Read, Eq)
derivePersistField "OracleSubmissionStatus"

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

data BreakSubmissionStatus = BreakPullFail | BreakPending | BreakTesting | BreakTested | BreakRejected | BreakJudging | BreakJudged | BreakTimeout
    deriving (Show, Read, Eq)
derivePersistField "BreakSubmissionStatus"

-- TODO: Collapse to a boolean. 
data BreakSubmissionResult = 
      BreakCorrect -- Valid bug
    | BreakIncorrect -- Invalid, correct behavior
    | BreakExploit -- Valid exploit
        deriving (Show, Read, Eq)
derivePersistField "BreakSubmissionResult"

data BreakType = 
      BreakCorrectness
    | BreakCrash
    | BreakConfidentiality
    | BreakIntegrity
        deriving (Show, Read, Eq)
derivePersistField "BreakType"

data FixSubmissionStatus = FixPending | FixBuilding | FixBuilt | FixJudging | FixJudged | FixTimeout | FixRejected
    | FixBuildFail | FixPullFail | FixInvalidBugId -- Deprecated.
    deriving (Show, Read, Eq)
derivePersistField "FixSubmissionStatus"

data FixSubmissionResult = FixFixed | FixNotFixed | FixDisqualified
    deriving (Show, Read, Eq)
derivePersistField "FixSubmissionResult"

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
    BreakJudging ->
        [shamlet|
            <span>
                Judging required
        |]
    BreakJudged ->
        [shamlet|
            <span>
                Judged
        |]

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
          BreakJudging ->
            "Judging required"
          BreakJudged ->
            "Judged"
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
    Just BreakIncorrect ->
        [shamlet|
            <span class="text-danger">
                Normal behavior
        |]
    Just BreakCorrect ->
        [shamlet|
            <span class="text-success">
                Bug
        |]
    Just BreakExploit ->
        [shamlet|
            <span class="text-success">
                Exploit
        |]
        
prettyBreakResultVictim :: Maybe BreakSubmissionResult -> Html
prettyBreakResultVictim s = case s of
    Nothing ->
        [shamlet|
            <span>
                &#8212;
        |]
    Just BreakIncorrect ->
        [shamlet|
            <span class="text-success">
                Normal behavior
        |]
    Just BreakCorrect ->
        [shamlet|
            <span class="text-danger">
                Bug
        |]
    Just BreakExploit ->
        [shamlet|
            <span class="text-danger">
                Exploit
        |]
        
prettyFixStatus :: FixSubmissionStatus -> Html
prettyFixStatus s = case s of 
    FixPullFail ->
        [shamlet|
            <span class="text-danger">
                Pull failed
        |]
    FixInvalidBugId ->
        [shamlet|
            <span class="text-danger">
                Invalid bug fixed
        |]
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
    FixBuildFail ->
        [shamlet|
            <span class="text-danger">
                Build failed
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
    Just FixDisqualified ->
        [shamlet|
            <span class="text-danger">
                Disqualified
        |]

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
prettyBreakType BreakConfidentiality = [shamlet|
        <span>
            Confidentiality
    |]
