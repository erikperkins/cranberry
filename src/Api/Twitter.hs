{-# LANGUAGE OverloadedStrings #-}

module Api.Twitter where

import System.Environment (getEnv)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import qualified Data.Conduit as C
import Data.Conduit.List as CL
import Web.Twitter.Conduit

import Network.AMQP
import qualified Data.Text as T (pack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, ByteString)
import Control.Monad (liftM)
import Web.Twitter.Types (StreamingAPI(..))
import Web.Twitter.Types (Status, statusText, rsRetweetedStatus)

twitterStream :: IO ()
twitterStream = do
  conn <- getConnection
  chan <- openChannel conn

  credential <- getCredential
  tokens <- getTokens
  let twInfo = setCredential tokens credential def

  feed <- getFeed
  manager <- newManager tlsManagerSettings

  -- Wrap this in a try statement, repeat if exception thrown
  runResourceT $ do
    src <- stream twInfo manager feed
    C.runConduit $ src C..| CL.mapM_ (liftIO . (consumeStream chan))

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

consumeStream :: Channel -> StreamingAPI -> IO ()
consumeStream chan tweet = do
  -- TODO: handle malformed tweets
  -- TODO: increment tweet count in redis
  case tweet of
    SStatus status -> do
      (publishTweet chan) . getTweetBody $ status

    SRetweetedStatus retweet -> do
      (publishTweet chan) . getTweetBody . rsRetweetedStatus $ retweet

    _ -> return ()


getTweetBody :: Status -> BL.ByteString
getTweetBody = BL.pack . show . statusText

publishTweet :: Channel -> BL.ByteString -> IO ()
publishTweet chan body = do
  let message = newMsg { msgBody = body, msgDeliveryMode = Just Persistent }
  publishMsg chan "" "tweets" message
  return ()
