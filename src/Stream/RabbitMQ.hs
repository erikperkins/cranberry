{-# LANGUAGE OverloadedStrings #-}

module Stream.RabbitMQ where

import Control.Monad (liftM)
import Data.Aeson (encode)
import Data.Map (fromList)
import GHC.Int (Int64)
import Network.AMQP
import Network.AMQP.Types (FieldTable(..), FieldValue(..))
import Stream.Twitter
import System.Environment (getEnv)

import qualified Data.Text as T (pack)


data RabbitMQConnection = RabbitMQConnection {
  connection :: Connection,
  channel :: Channel
}

close :: RabbitMQConnection -> IO ()
close = do
  closeConnection . connection

getRabbitMQConnection :: IO RabbitMQConnection
getRabbitMQConnection = do
  conn <- getConnection
  chan <- openChannel conn

  messageTtl <- liftM (\a -> read a :: Int64) (getEnv "RABBITMQ_MESSAGE_TTL")
  let headers = FieldTable $ fromList [("x-message-ttl", FVInt64 messageTtl)]

  let options = newQueue {
    queueName = "tweets",
    queueAutoDelete = False,
    queueDurable = False,
    queueExclusive = False,
    queuePassive = False,
    queueHeaders = headers
  }

  deleteQueue chan "tweets"
  declareQueue chan options

  return RabbitMQConnection {
    connection = conn,
    channel = chan
  }


getConnection :: IO Connection
getConnection = do
  rabbitmqHost <- getEnv "RABBITMQ_HOST"
  rabbitmqUser <- liftM T.pack (getEnv "RABBITMQ_USERNAME")
  rabbitmqPassword <- liftM T.pack (getEnv "RABBITMQ_PASSWORD")
  openConnection rabbitmqHost "/" rabbitmqUser rabbitmqPassword


publishTweet :: Channel -> StrippedTweet -> IO StrippedTweet
publishTweet chan tweet = do
  let body = encode tweet
  let message = newMsg { msgBody = body, msgDeliveryMode = Just Persistent }
  publishMsg chan "" "tweets" message
  return tweet
