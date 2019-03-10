{-# LANGUAGE OverloadedStrings #-}

module Stream.Twitter where

import Control.Monad (liftM)
import Data.Aeson
import Data.ByteString.UTF8 (fromString)
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Environment (getEnv)
import Web.Twitter.Conduit
import Web.Twitter.Types (Status(..), RetweetedStatus(..), User(..))
import Web.Twitter.Types (StreamingAPI(..))

import qualified Data.Text as T (pack, Text)


data TwitterConnection = TwitterConnectInfo {
  twInfo :: TWInfo,
  manager :: Manager,
  feed :: APIRequest StatusesFilter StreamingAPI
}


data StrippedTweet = StrippedTweet {
  tweetId :: Integer,
  screenName :: T.Text,
  createdAt :: UTCTime,
  text :: T.Text
} deriving Show


instance ToJSON StrippedTweet where
  toJSON (StrippedTweet tweetId' screenName' createdAt' text') =
    object [
      "tweet_id" .= tweetId',
       "screen_name" .= screenName',
      "created_at" .= twitterTimestamp createdAt',
      "text" .= text'
    ]


twitterTimestamp :: UTCTime -> String
twitterTimestamp = formatTime defaultTimeLocale "%a %b %d %H:%M:%S %z %Y"


getTwitterConnection :: IO TwitterConnection
getTwitterConnection = do
  twInfo' <- getTWInfo
  manager' <- newManager tlsManagerSettings
  feed' <- getFeed

  return TwitterConnectInfo {
    twInfo = twInfo',
    manager = manager',
    feed = feed'
  }


getTWInfo :: IO TWInfo
getTWInfo = do
  credential <- getCredential
  tokens <- getTokens
  return $ setCredential tokens credential def


getCredential :: IO Credential
getCredential = do
  accessToken <- liftM fromString (getEnv "TWITTER_ACCESS_TOKEN")
  accessSecret <- liftM fromString (getEnv "TWITTER_ACCESS_SECRET")
  let oauthToken = fromString "oauth_token"
  let oauthTokenSecret = fromString "oauth_token_secret"
  return $
    Credential [(oauthToken, accessToken), (oauthTokenSecret, accessSecret)]


getTokens :: IO OAuth
getTokens = do
  consumerKey <- liftM fromString (getEnv "TWITTER_CONSUMER_KEY")
  consumerSecret <- liftM fromString (getEnv "TWITTER_CONSUMER_SECRET")
  return $ twitterOAuth {
    oauthConsumerKey = consumerKey,
    oauthConsumerSecret = consumerSecret
  }


getFeed :: IO (APIRequest StatusesFilter StreamingAPI)
getFeed = do
  track <- liftM T.pack (getEnv "TWITTER_TRACK")
  return $ statusesFilterByTrack $ track


stripTweet :: Status -> StrippedTweet
stripTweet tweet = do
  StrippedTweet {
    tweetId = statusId tweet,
    screenName = userScreenName . statusUser $ tweet,
    createdAt = statusCreatedAt tweet,
    text = statusText tweet
  }


stripRetweet :: RetweetedStatus -> StrippedTweet
stripRetweet retweet = do
  StrippedTweet {
    tweetId = statusId . rsRetweetedStatus $ retweet,
    screenName = userScreenName . statusUser . rsRetweetedStatus $ retweet,
    createdAt = rsCreatedAt retweet,
    text = statusText . rsRetweetedStatus $ retweet
  }
