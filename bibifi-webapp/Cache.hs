{-# LANGUAGE ScopedTypeVariables #-}

module Cache (buildersCode) where

import Control.Monad
import qualified Data.List as List
import Data.Bits
import Data.Ord
import Data.Time.Clock
import qualified Database.Esqueleto as E

import Import
import PostDependencyType

-- Checks if the cache for the given key is expired. Updates the expiration date by the given period.
isExpired :: Text -> NominalDiffTime -> LHandler Bool
isExpired key period = do
    now <- lLift $ lift getCurrentTime
    mCache <- runDB $ getBy $ UniqueCache key
    case mCache of
        Nothing ->
            let newExpiration = addUTCTime period now in
            do
            runDB $ insert_ $ CacheExpiration key newExpiration
            return True
        Just (Entity cacheId (CacheExpiration _ expiration)) ->
            if now > expiration then
                let newExpiration = addUTCTime period now in
                do
                runDB $ update cacheId [CacheExpirationExpiration =. newExpiration]
                return True
            else
                return False

--buildersCode :: ContestId -> SelectOpt CacheBuildersCode -> Handler [Entity CacheBuildersCode]
--buildersCode :: Ord o => ContestId -> (CacheBuildersCode -> o) -> Handler [Entity CacheBuildersCode]
buildersCode contestId direction tcId = 
    let updateCache = runDB $ do
          -- Query inspired from: http://stackoverflow.com/questions/8748986/get-records-with-highest-smallest-whatever-per-group/8749095#8749095
          builderScores <- E.select $ E.from $ \( tc `E.InnerJoin` (tbs `E.LeftOuterJoin` tbs')) -> do
            E.on ( E.just (tbs E.^. TeamBuildScoreTeam) E.==. tbs' E.?. TeamBuildScoreTeam E.&&. E.just (tbs E.^. TeamBuildScoreTimestamp) E.<. tbs' E.?. TeamBuildScoreTimestamp)
            E.on ( tc E.^. TeamContestId E.==. tbs E.^. TeamBuildScoreTeam)
            E.where_ ( tc E.^. TeamContestContest E.==. E.val contestId E.&&. E.isNothing (tbs' E.?. TeamBuildScoreTeam) E.&&. tc E.^. TeamContestProfessional E.==. E.val False)
            E.orderBy [ E.asc ( tbs E.^. TeamBuildScoreTimestamp)]
            return tbs
          -- Delete old cache. Not sure if this is the best way to do this...
          deleteWhere [CacheBuildersCodeContestId ==. contestId]
          -- Iterate over new builder scores, updating accordingly. 
          mapM_ (\(Entity _ (TeamBuildScore teamId tbsBS tbsRS tbsFS _)) -> 
                -- Filter out teams that don't make it pass the first round.
                case tbsBS of
                    Just x | x > 0.0 ->
                        let mDouble mX = case mX of 
                              Nothing -> 0.0
                              Just x -> x
                        in
                        -- Compute latest build score.
                        let buildScore = case tbsBS of
                              Nothing -> 
                                0.0
                              _ ->
                                (mDouble tbsBS) + (mDouble tbsRS) + (mDouble tbsFS) 
                        in
                        do
                        -- -- Get latest break score.
                        -- breakScore <- do
                        --     mBreakScore <- selectFirst [TeamBreakScoreTeam ==. teamId] [Desc TeamBreakScoreTimestamp]
                        --     return $ case mBreakScore of
                        --         Nothing ->
                        --             0.0
                        --         Just (Entity _ breakScore) ->
                        --             (mDouble (teamBreakScoreBreakScore breakScore)) + (mDouble (teamBreakScoreFixScore breakScore))
                        -- Get number of bugs found in their code.
                        bugCount <- count [ BreakSubmissionTargetTeam ==. teamId, BreakSubmissionStatus ==. BreakTested, BreakSubmissionResult ==. Just BreakCorrect]
                        -- Get number of vulnerabilities found in their code.
                        vulnerabilityCount <- count [ BreakSubmissionTargetTeam ==. teamId, BreakSubmissionStatus ==. BreakTested, BreakSubmissionResult ==. Just BreakExploit]

                        -- Get team name and languages.
                        (name, languages) <- do
                            lTeam <- E.select $ E.from $ \ (t `E.InnerJoin` tc) -> do
                                E.on ( t E.^. TeamId E.==. tc E.^. TeamContestTeam)
                                E.where_ ( tc E.^. TeamContestId E.==. E.val teamId)
                                E.limit 1
                                return ( t E.^. TeamName, tc E.^. TeamContestLanguages)
                            return $ case lTeam of
                                [(name,languages)] -> (E.unValue name, E.unValue languages)
                                _ -> ("Not found","")

                        -- Update the cached value.
                        insert_ $ CacheBuildersCode name teamId languages contestId buildScore bugCount vulnerabilityCount
                    _ ->
                        return ()
            ) builderScores
    in
    -- Cache for 2 minutes.
    let cacheTime = fromIntegral $ 60 * 2 in
    do
    expired <- isExpired "buildersCode" $ cacheTime
    when expired updateCache
    --runDB $ selectList [CacheBuildersCodeContestId ==. contestId] [direction]
    res :: [Entity CacheBuildersCode] <- runDB $ selectList [CacheBuildersCodeContestId ==. contestId] []
    return $ List.sortBy (\(Entity aId a) (Entity bId b) -> case direction a b of
            EQ ->
                let breakTie i = xor (keyToInt i) (keyToInt tcId) in
                comparing breakTie aId bId
            ord ->
                ord
        ) res
    --runDB $ rawSql "SELECT ?? FROM \"cache_builders_code\" WHERE (\"contest_id\"=?) ORDER BY ?" [toPersistValue contestId, direction]

