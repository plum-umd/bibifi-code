import Prelude
import qualified System.Environment as SE
import qualified Data.Char as C
import Common
import qualified Extract
import qualified Judgements
import qualified Migrate
import qualified Request
import qualified Retrieve
import qualified Submit
import qualified Tests
import qualified Rescore

import Database.Persist

main :: IO ()
main = do
    args' <- SE.getArgs
    case args' of
        command:args ->
            case lookup (fmap C.toLower command) dispatch of
                Nothing ->
                    usage
                Just cmd -> do
                    -- Create database pool and config.
                    db <- makeDatabaseConf "/fs/mc2-application/config/postgresql.yml" "Translator"
                    -- db <- makeDatabaseConf productionDatabaseYML "Translator"
                    runDatabaseM db $ cmd args
        _ -> 
            usage

dispatch :: [(String, [String] -> DatabaseM ())]  
dispatch = [( "request", Request.request), ( "submit", Submit.submit), ( "retrieve", Retrieve.retrieve), ( "tests", Tests.tests), ( "rescore", Rescore.rescore), ( "preparejudgements", Judgements.prepare), ("migrate", Migrate.migrate), ("extract", Extract.extract)]

usage :: MonadIO m => m ()
usage = 
    silentFail "usage: ./translator RETRIEVE|REQUEST|SUBMIT|TESTS|RESCORE|PREPAREJUDGEMENTS|MIGRATE|EXTRACT"
