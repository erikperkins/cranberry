{-# LANGUAGE OverloadedStrings #-}

module Stream.Redis where

import Control.Monad (liftM)
import Data.ByteString.Char8 (pack, ByteString)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.Redis hiding (time)
import Stream.Twitter (StrippedTweet(..))
import System.Environment (getEnv)


type RedisConnection = Connection


getRedisConnection :: IO Connection
getRedisConnection = do
  host <- getEnv "REDIS_HOST"
  database <- liftM read (getEnv "REDIS_DATABASE")
  password <- liftM pack (getEnv "REDIS_PASSWORD")

  let connectInfo = defaultConnectInfo {
    connectHost = host,
    connectDatabase = database,
    connectAuth = Just password
  }

  redis <- connect connectInfo
  setTrack redis
  return redis


setTrack :: Connection -> IO ()
setTrack redis = do
  track <- liftM pack (getEnv "TWITTER_TRACK")
  _ <- runRedis redis $ set "twitter:track" track
  return ()


incrementTweets :: StrippedTweet -> Connection -> IO ()
incrementTweets chirp redis = do
  let key = posixDay . createdAt $ chirp
  let field = posixMinute . createdAt $ chirp
  runRedis redis $ do
    multiExec $ do
      incrementField <- hincrby key field (1 :: Integer)
      expireKey <- expire key 3600
      return $ (,) <$> incrementField <*> expireKey
  return ()


posixDay :: UTCTime -> ByteString
posixDay time = do
  let utcTime = utcToLocalTime utc time
  let utcDay = utcTime { localTimeOfDay = midnight }
  let key = init . show . utcTimeToPOSIXSeconds $ localTimeToUTC utc $ utcDay
  pack $ "tweet:" ++ key


posixMinute :: UTCTime -> ByteString
posixMinute time = do
  let utcTime = utcToLocalTime utc time
  let utcTimeOfDay = localTimeOfDay utcTime
  let utcMinute = utcTime { localTimeOfDay = utcTimeOfDay { todSec = 0 } }
  pack . init . show . utcTimeToPOSIXSeconds $ localTimeToUTC utc $ utcMinute
