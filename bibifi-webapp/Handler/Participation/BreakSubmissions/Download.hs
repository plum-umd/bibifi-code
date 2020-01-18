module Handler.Participation.BreakSubmissions.Download where

import BreakSubmissions (checkBreakSubmissionTeam)
import Import
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Participation


getParticipationBreakSubmissionDownloadR :: TeamContestId -> BreakSubmissionId -> Handler Html
getParticipationBreakSubmissionDownloadR tcId bsId = runLHandler $ do
    _bs <- checkBreakSubmissionTeam tcId bsId
    Participation.layout Participation.BreakSubmissions tcId $ \_userId _teamcontest _contest _team -> handlerToWidget $ do

        content <- (breakSubmissionFileFile . entityVal) <$> runDB (getBy404 $ UniqueBreakSubmissionFile bsId)
        
        -- Deliver the file.
        addHeader "Content-Disposition" $ Text.concat [ "attachment; filename=\"", name, "\""]
        sendResponse ( Text.encodeUtf8 contentType, toContent content)

    where
        name = Text.pack $ show (keyToInt tcId) <> "-" <> show (keyToInt bsId) <> ".tar.gz"
        contentType = "application/gzip"
