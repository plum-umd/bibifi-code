module Handler.Participation.DownloadBuilder where

import BuildSubmissions
import Import
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Participation

getParticipationDownloadBuilderR :: TeamContestId -> TeamContestId -> Handler TypedContent
getParticipationDownloadBuilderR dlId tcId = runLHandler $ do
    raiseTeamLabel
    raiseGroupLabel
    (_, _, contest, _) <- Participation.checkCreds dlId

    -- Check if break/fix-it round has started.
    now <- getCurrentTime
    if now < contestBreakFixStart contest then
        notFound
    else do
        content <- runDB $ do
            submissionE <- getLatestBuildOrFix contest tcId now
            case submissionE of
                Left _ ->
                    notFound
                Right submissionE -> case submissionE of
                    Left _bsId ->
                        (buildSubmissionFileFile . entityVal) <$> getBy404 (UniqueBuildSubmissionFile tcId)
                    Right fsId ->
                        (fixSubmissionFileFile . entityVal) <$> getBy404 (UniqueFixSubmissionFile fsId)
    
        -- Deliver the file.
        addHeader "Content-Disposition" $ Text.concat [ "attachment; filename=\"", name, "\""]
        sendResponse ( Text.encodeUtf8 contentType, toContent content)

    where
        name = Text.pack $ show (keyToInt tcId) <> ".tar.gz"
        contentType = "application/gzip"
