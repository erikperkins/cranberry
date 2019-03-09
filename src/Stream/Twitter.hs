{-# LANGUAGE OverloadedStrings #-}

module Stream.Twitter where

import Control.Monad (liftM)
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Environment (getEnv)
import Web.Twitter.Conduit
import Web.Twitter.Types (Status(..), StreamingAPI(..), RetweetedStatus(..))

import qualified Data.Text as T (pack, Text)


data TwitterConnection = TwitterConnectInfo {
  twInfo :: TWInfo,
  manager :: Manager,
  feed :: APIRequest StatusesFilter StreamingAPI
}


data StrippedTweet = StrippedTweet {
  createdAt :: UTCTime,
  text :: T.Text
} deriving Show


instance ToJSON StrippedTweet where
  toJSON (StrippedTweet createdAt' text') =
    object ["created_at" .= twitterTimestamp createdAt', "text" .= text']


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


getFeed :: IO (APIRequest StatusesFilter StreamingAPI)
getFeed = do
  track <- liftM T.pack (getEnv "TWITTER_TRACK")
  return $ statusesFilterByTrack $ track


stripTweet :: Status -> StrippedTweet
stripTweet tweet = do
  let body = statusText $ tweet
  let created = statusCreatedAt $ tweet
  StrippedTweet { createdAt = created, text = body }


stripRetweet :: RetweetedStatus -> StrippedTweet
stripRetweet retweet = do
  let tweet = rsRetweetedStatus retweet
  let body = statusText tweet
  let created = rsCreatedAt retweet
  StrippedTweet { createdAt = created, text = body }
