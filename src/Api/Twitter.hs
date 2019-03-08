{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Twitter where

import System.Environment (getEnv)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import qualified Data.Conduit as C
import Data.Conduit.List as CL
import Web.Twitter.Conduit
import Network.AMQP
import qualified Data.Text as T (pack, Text)
import qualified Data.ByteString.Lazy.UTF8 as BL (ByteString)
import Control.Monad (liftM)
import Web.Twitter.Types (Status, StreamingAPI(..))
import Web.Twitter.Types (statusText, statusCreatedAt, rsRetweetedStatus)
import GHC.Generics
import Data.Aeson
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad.Trans.Resource (MonadThrow, MonadUnliftIO)
import Control.Exception (catch)
import Data.Conduit.Attoparsec (ParseError)

data StrippedTweet = StrippedTweet {
  created_at :: String,
  text :: T.Text
} deriving (Generic, Show)

instance ToJSON StrippedTweet where
  toEncoding = genericToEncoding defaultOptions

twitterStream :: IO ()
twitterStream = do
  twInfo <- getTWInfo
  manager <- newManager tlsManagerSettings
  feed <- getFeed
  conn <- getConnection
  chan <- openChannel conn
  catch (consumeStream twInfo manager feed chan) $
    \e -> do
      print (e :: ParseError)
      closeConnection conn
      twitterStream

consumeStream :: (MonadThrow m, MonadUnliftIO m) =>
  TWInfo -> Manager -> (APIRequest StatusesFilter StreamingAPI)
    -> Channel -> m ()
consumeStream twInfo manager feed chan = do
  runResourceT $ do
    src <- stream twInfo manager feed
    C.runConduit $ src C..| CL.mapM_ (liftIO . (handleStream chan))

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

handleStream :: Channel -> StreamingAPI -> IO ()
handleStream chan tweet = do
  case tweet of
    SStatus status -> do
      (publishTweet chan) . stripTweet $ status
      -- TODO: Increment redis key here instead of in bayberry

    SRetweetedStatus retweet -> do
      (publishTweet chan) . stripTweet . rsRetweetedStatus $ retweet
      -- TODO: Increment redis key here instead of in bayberry

    _ -> return ()

stripTweet :: Status -> BL.ByteString
stripTweet tweet = do
  let body = statusText $ tweet
  let format = "%a %b %d %H:%M:%S %z %Y"
  let created = (formatTime defaultTimeLocale format) . statusCreatedAt $ tweet
  encode $ StrippedTweet { created_at = created, text = body }

publishTweet :: Channel -> BL.ByteString -> IO ()
publishTweet chan body = do
  let message = newMsg { msgBody = body, msgDeliveryMode = Just Persistent }
  publishMsg chan "" "tweets" message
  return ()
