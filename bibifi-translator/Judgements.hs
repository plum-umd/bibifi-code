module Judgements where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Char as C
import Data.Text (Text(..))
import qualified Data.Text as T
import Database.Esqueleto

import Common

prepare :: Entity Contest -> [String] -> DatabaseM ()
prepare c args' = case args' of
    cmd':[] ->
        case lookup (fmap C.toLower cmd') dispatch of
            Nothing ->
                usage
            Just cmd ->
                cmd c
    _ ->
        usage

dispatch :: [(String, Entity Contest -> DatabaseM ())]
dispatch = [ ( "round1", round1), ( "round2", round2), ( "round3", round3)]

usage :: DatabaseM ()
usage = silentFail $ usageDispatch "PREPAREJUDGEMENT" dispatch

data RetSubmission = RetSubmission { 
        teamId :: Key TeamContest, 
        hash :: Text,
        repoLocation :: String
    }

instance ToJSON RetSubmission where
    toJSON (RetSubmission teamId hash repoLocation) = object [
            "teamId" .= teamId,
            "hash" .= hash,
            "repoLocation" .= repoLocation
        ]


round1 (Entity cId _) = do
    -- Get judgements for given contest.
    res' <- runDB $ select $ from $ \(InnerJoin bj (InnerJoin bs tc)) -> do
        on (bs ^. BuildSubmissionTeam ==. tc ^. TeamContestId)
        on (bj ^. BuildJudgementSubmission ==. bs ^. BuildSubmissionId)
        where_ (tc ^. TeamContestContest ==. val cId)
        return ( tc ^. TeamContestId, bs ^. BuildSubmissionCommitHash, bj ^. BuildJudgementId)
    let res = map (\(Value teamId, Value hash, Value judgementId) -> 
            let repoLocation = "/build/" ++ (show ( keyToInt judgementId)) in
            RetSubmission teamId hash repoLocation
          ) res'
    liftIO $ BS.putStrLn $ encode res

round2 _ = silentFail "TODO"
round3 _ = silentFail "TODO"
