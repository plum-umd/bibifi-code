import qualified Data.Char as C
import Database.Persist
import Prelude
import qualified System.Environment as SE
import qualified Data.Text as Text
import Common
import qualified Extract
import qualified Dump
import qualified Judgements
import qualified Migrate
import qualified Request
import qualified Retrieve
import qualified Submit
import qualified Tests
import qualified Rescore

main :: IO ()
main = do
    args' <- SE.getArgs
    -- Create database pool and config.
    db <- makeDatabaseConf "/home/bibifi/bibifi-code/config/postgresql.yml" "Translator"
    runDatabaseM db $ case args' of
        "-c":contest_url:command:args -> do
            contestM <- runDB $ getBy $ UniqueContest $ Text.pack contest_url

            case contestM of
                Nothing ->
                    silentFail $ "Contest not found: " ++ contest_url
                Just contest ->
                    run contest db command args

        command:args -> do
            putLog "Warning: No contest specified. Assuming default contest."
            contest <- activeContest

            run contest db command args
        _ -> 
            usage

    where
        run contest db command args = case lookup (fmap C.toLower command) dispatch of
            Nothing ->
                usage
            Just cmd -> do
                -- db <- makeDatabaseConf productionDatabaseYML "Translator"
                -- db <- makeDatabaseConf productionDatabaseYML "Translator"
                cmd contest args
            

dispatch :: [(String, Entity Contest -> [String] -> DatabaseM ())]  
dispatch = [( "request", Request.request), ( "submit", Submit.submit), ( "retrieve", Retrieve.retrieve), ( "tests", Tests.tests), ( "rescore", Rescore.rescore), ( "preparejudgements", Judgements.prepare), ("migrate", Migrate.migrate), ("extract", Extract.extract), ("dump", Dump.dump)]

usage :: MonadIO m => m ()
usage = 
    silentFail "usage: ./translator RETRIEVE|REQUEST|SUBMIT|TESTS|RESCORE|PREPAREJUDGEMENTS|MIGRATE|EXTRACT|DUMP"
