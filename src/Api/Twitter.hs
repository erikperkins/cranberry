{-# LANGUAGE OverloadedStrings #-}

module Api.Twitter where

import System.Environment (getEnv)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import qualified Data.Conduit as C
import Data.Conduit.List as CL
import Web.Twitter.Conduit

-- import Network.AMQP
import qualified Data.Text as T (pack)
-- import qualified Data.ByteString.Lazy.Char8 as BL (pack, ByteString)
import Control.Monad (liftM)
import Web.Twitter.Types (StreamingAPI(..))
-- import Web.Twitter.Types (Status, statusText) --, rsRetweetedStatus)

-- import Control.Exception (try)

twitterStream :: IO ()
twitterStream = do
--   conn <- getConnection
--   chan <- openChannel conn
--
--   let queue = newQueue { queueName = "cranberry_queue" }
--   let exchange = newExchange {
--     exchangeName = "cranberry_exchange",
--     exchangeType = "direct"
--   }
--
--   declareQueue chan queue
--   declareExchange chan exchange
--   bindQueue chan "cranberry_queue" "cranberry_exchange" "cranberry_key"

  credential <- getCredential
  tokens <- getTokens

--   accessToken <- getEnv "TWITTER_ACCESS_TOKEN"
--   accessSecret <- getEnv "TWITTER_ACCESS_SECRET"
--   let credential = Credential [(pack "oauth_token", pack accessToken), (pack "oauth_token_secret", pack accessSecret)]

--   consumerKey <- getEnv "TWITTER_CONSUMER_KEY"
--   consumerSecret <- getEnv "TWITTER_CONSUMER_SECRET"
--   let tokens = twitterOAuth {oauthConsumerKey = pack consumerKey, oauthConsumerSecret = pack consumerSecret}

  let twInfo = setCredential tokens credential def

  feed <- getFeed

  manager <- newManager tlsManagerSettings

  runResourceT $ do
    src <- stream twInfo manager feed
    C.runConduit $ src C..| CL.mapM_ (liftIO . print)

--   runResourceT $ do
--     src <- stream twInfo manager potus
--     C.runConduit $ src C..| CL.mapM_ (liftIO . (consumeStream chan))


-- getConnection :: IO Connection
-- getConnection = do
--   rabbitmqHost <- getEnv "RABBITMQ_HOST"
--   rabbitmqUser <- (liftM T.pack) $ getEnv "RABBITMQ_USERNAME"
--   rabbitmqPassword <- (liftM T.pack) $ getEnv "RABBITMQ_PASSWORD"
--   openConnection rabbitmqHost "/" rabbitmqUser rabbitmqPassword

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
  return $ Credential [(oauthToken, accessToken), (oauthTokenSecret, accessSecret)]


getTokens :: IO OAuth
getTokens = do
  consumerKey <- liftM pack (getEnv "TWITTER_CONSUMER_KEY")
  consumerSecret <- liftM pack (getEnv "TWITTER_CONSUMER_SECRET")
  return $ twitterOAuth {oauthConsumerKey = consumerKey, oauthConsumerSecret = consumerSecret}


-- consumeStream :: Channel -> StreamingAPI -> IO ()
-- consumeStream chan tweet = do
--   -- TODO: handle malformed tweets
--   case tweet of
--     SStatus status -> (publishTweet chan) . getTweetBody $ status
--     SRetweetedStatus retweet ->
--       (publishTweet chan) . getTweetBody . rsRetweetedStatus $ retweet
--     _ -> return ()
--   -- TODO: increment tweet count in redis

-- getTweetBody :: Status -> BL.ByteString
-- getTweetBody = BL.fromString . show . statusText
--
-- publishTweet :: Channel -> BL.ByteString -> IO ()
-- publishTweet chan body = do
--   let message = newMsg { msgBody = body, msgDeliveryMode = Just Persistent }
--   publishMsg chan "cranberry_exchange" "cranberry_key" message
--   return ()
