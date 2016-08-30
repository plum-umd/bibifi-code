{-# LANGUAGE FunctionalDependencies #-}
module RateLimit (RateLimiter(..), getIPAddress) where

import qualified Data.ByteString.Char8 as BSC
import Data.Hashable
import qualified Data.List as List
import Data.Textual (fromString)
import Database.Persist
import Database.Persist.RateLimit
import Network.IP.Addr (unIP4)
import Network.Socket
import Network.Wai
import Prelude
import Yesod.Core.Handler

import Model

data RateLimiter = 
      RateLimitRegister SockAddr
    | RateLimitLoginAttempt SockAddr
--     | RateLimitOracleTest TeamContestId

instance Hashable SockAddr where
    hashWithSalt salt (SockAddrInet _ addr) = 
        hashWithSalt (hashWithSalt salt (0::Int)) addr
    hashWithSalt salt (SockAddrInet6 _ _ addr _) =
        hashWithSalt (hashWithSalt salt (1::Int)) addr
    hashWithSalt salt (SockAddrUnix addr) = 
        hashWithSalt (hashWithSalt salt (2::Int)) addr

getIPAddress :: HandlerT site IO SockAddr
getIPAddress = do
    req <- fmap reqWaiRequest getRequest
    case List.lookup "X-Forwarded-For" $ requestHeaders req of
        Just ipS -> case fromString $ BSC.unpack ipS of
            Just ip4 ->
                return $ SockAddrInet 443 $ unIP4 ip4
            Nothing -> do
                return $ remoteHost req
        Nothing -> do
            return $ remoteHost req
    
getIdentifier :: RateLimiter -> Int
getIdentifier (RateLimitRegister _) = 1
getIdentifier (RateLimitLoginAttempt _) = 2

getLimiter :: RateLimiter -> Int
getLimiter (RateLimitRegister ip) = hash ip
getLimiter (RateLimitLoginAttempt ip) = hash ip

instance RateLimit RateLimiter RateLimitLog where
    rateLimit (RateLimitRegister _) = ( 3, 60*60)
    rateLimit (RateLimitLoginAttempt _) = ( 100, 60*60)
    convertAction action time = 
        let identifier = getIdentifier action in
        let limiter = getLimiter action in
        RateLimitLog identifier limiter time
    timeConstructor _ = RateLimitLogTime
    deleteFilters action = 
        [RateLimitLogAction ==. (getIdentifier action)]
    rateLimitFilters action = 
        let filters' = deleteFilters action in
        (RateLimitLogLimiter ==. getLimiter action):filters'

