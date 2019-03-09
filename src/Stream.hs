{-# LANGUAGE OverloadedStrings #-}

module Stream where

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource (MonadThrow, MonadUnliftIO)
import Data.Conduit.Attoparsec (ParseError)
import Database.Redis hiding (stream)
import Stream.RabbitMQ
import Stream.Redis
import Stream.Twitter
import Web.Twitter.Conduit
import Web.Twitter.Types (StreamingAPI(..))

import qualified Data.Conduit as C
import qualified Data.Conduit.List as L (mapM_)


receive :: IO ()
receive = do
  twitter <- getTwitterConnection
  rabbitmq <- getRabbitMQConnection
  redis <- getRedisConnection

  catch (consume twitter rabbitmq redis) $
    \e -> do
      print (e :: ParseError)
      close rabbitmq
      disconnect redis
      receive


consume :: (MonadThrow m, MonadUnliftIO m) =>
  TwitterConnection -> RabbitMQConnection -> RedisConnection -> m ()
consume twitter rabbitmq redis = do
  runResourceT $ do
    src <- stream (twInfo twitter) (manager twitter) (feed twitter)
    C.runConduit $ src C..| L.mapM_ (liftIO . (handle rabbitmq redis))


handle :: RabbitMQConnection -> RedisConnection -> StreamingAPI -> IO ()
handle rabbitmq redis tweet = do
  let chan = channel rabbitmq

  case tweet of
    SStatus status -> do
      chirp <- (publishTweet chan) . stripTweet $ status
      incrementTweets chirp redis

    SRetweetedStatus status -> do
      chirp <- (publishTweet chan) . stripRetweet $ status
      incrementTweets chirp redis

    _ -> return ()
