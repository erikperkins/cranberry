{-# LANGUAGE OverloadedStrings #-}

module Api.Twitter where

import Control.Monad (liftM)
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource (MonadThrow, MonadUnliftIO)
import Data.Aeson
import Data.ByteString.Char8 (pack, ByteString)
import Data.Conduit.Attoparsec (ParseError)
import Data.Conduit.List as CL
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.AMQP
import System.Environment (getEnv)
import Web.Twitter.Conduit
import Web.Twitter.Types (Status, StreamingAPI(..))
import Web.Twitter.Types (statusText, statusCreatedAt, rsRetweetedStatus)

import qualified Data.Conduit as C
import qualified Data.Text as T (pack, Text)
import qualified Database.Redis as R


data StrippedTweet = StrippedTweet {
  createdAt :: UTCTime,
  text :: T.Text
} deriving Show


instance ToJSON StrippedTweet where
  toJSON (StrippedTweet c t) =
    object ["created_at" .= twitterTime c, "text" .= t]


twitterStream :: IO ()
twitterStream = do
  twInfo <- getTWInfo
  manager <- newManager tlsManagerSettings
  feed <- getFeed
  conn <- getConnection
  chan <- openChannel conn
  redis <- getRedis

  catch (consumeStream twInfo manager feed chan redis) $
    \e -> do
      print (e :: ParseError)
      closeConnection conn
      R.disconnect redis
      twitterStream


consumeStream :: (MonadThrow m, MonadUnliftIO m) =>
  TWInfo -> Manager -> (APIRequest StatusesFilter StreamingAPI)
    -> Channel -> R.Connection -> m ()
consumeStream twInfo manager feed chan redis = do
  runResourceT $ do
    src <- stream twInfo manager feed
    C.runConduit $ src C..| CL.mapM_ (liftIO . (handleStream chan redis))


getRedis :: IO R.Connection
getRedis = do
  host <- getEnv "REDIS_HOST"
  database <- liftM read (getEnv "REDIS_DATABASE")
  password <- liftM pack (getEnv "REDIS_PASSWORD")

  let connectInfo = R.defaultConnectInfo {
    R.connectHost = host,
    R.connectDatabase = database,
    R.connectAuth = Just password
  }
  R.connect connectInfo


getTWInfo :: IO TWInfo
getTWInfo = do
  credential <- getCredential
  tokens <- getTokens
  return $ setCredential tokens credential def


getConnection :: IO Connection
getConnection = do
  rabbitmqHost <- getEnv "RABBITMQ_HOST"
  rabbitmqUser <- liftM T.pack (getEnv "RABBITMQ_USERNAME")
  rabbitmqPassword <- liftM T.pack (getEnv "RABBITMQ_PASSWORD")
  openConnection rabbitmqHost "/" rabbitmqUser rabbitmqPassword


getFeed :: IO (APIRequest StatusesFilter StreamingAPI)
getFeed = do
  track <- liftM T.pack (getEnv "TWITTER_TRACK")
  return $ statusesFilterByTrack $ track


getCredential :: IO Credential
getCredential = do
  accessToken <- liftM pack (getEnv "TWITTER_ACCESS_TOKEN")
  accessSecret <- liftM pack (getEnv "TWITTER_ACCESS_SECRET")
  let oauthToken = pack "oauth_token"
  let oauthTokenSecret = pack "oauth_token_secret"
  return $
    Credential [(oauthToken, accessToken), (oauthTokenSecret, accessSecret)]


getTokens :: IO OAuth
getTokens = do
  consumerKey <- liftM pack (getEnv "TWITTER_CONSUMER_KEY")
  consumerSecret <- liftM pack (getEnv "TWITTER_CONSUMER_SECRET")
  return $ twitterOAuth {
    oauthConsumerKey = consumerKey,
    oauthConsumerSecret = consumerSecret
  }


handleStream :: Channel -> R.Connection -> StreamingAPI -> IO ()
handleStream chan redis tweet = do
  case tweet of
    SStatus status -> do
      chirp <- (publishTweet chan) . stripTweet $ status
      incrementTweets chirp redis

    SRetweetedStatus retweet -> do
      chirp <- (publishTweet chan) . stripTweet . rsRetweetedStatus $ retweet
      incrementTweets chirp redis

    _ -> return ()


incrementTweets :: StrippedTweet -> R.Connection -> IO ()
incrementTweets chirp redis = do
  let key = posixDay . createdAt $ chirp
  let field = posixMinute . createdAt $ chirp
  R.runRedis redis $ R.hincrby key field (1 :: Integer)
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


publishTweet :: Channel -> StrippedTweet -> IO StrippedTweet
publishTweet chan tweet = do
  let body = encode tweet
  let message = newMsg { msgBody = body, msgDeliveryMode = Just Persistent }
  publishMsg chan "" "tweets" message
  return tweet


stripTweet :: Status -> StrippedTweet
stripTweet tweet = do
  let body = statusText $ tweet
  let created = statusCreatedAt $ tweet
  StrippedTweet { createdAt = created, text = body }


twitterTime :: UTCTime -> String
twitterTime = formatTime defaultTimeLocale "%a %b %d %H:%M:%S %z %Y"
