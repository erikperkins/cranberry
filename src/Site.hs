{-# LANGUAGE OverloadedStrings #-}

module Site (cranberry) where

import Api.Forecast (forecastInit)
import Application
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Database.Redis (ConnectInfo, defaultConnectInfo)
import Database.Redis (connectDatabase, connectHost, connectAuth)
import Snap.Core (writeBS, getParam)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, nestSnaplet, SnapletInit)
import Snap.Snaplet.RedisDB
import System.Environment (getEnv)


cranberry :: SnapletInit App App
cranberry = makeSnaplet "app" "Snaplet example application" Nothing $ do
  connection <- liftIO getConnectInfo
  redisSnaplet <- nestSnaplet "redis" redis $ redisDBInit connection
  forecastSnaplet <- nestSnaplet "forecast" forecast $ forecastInit redisSnaplet
  addRoutes routes
  return $ App forecastSnaplet redisSnaplet


routes :: [(ByteString, Handler App App ())]
routes = [
    ("/", root),
    ("echo/:slug", echo)
  ]


root :: AppHandler ()
root = writeBS "Hello, Snap!\n"


echo :: AppHandler ()
echo = do
  slug <- getParam "slug"
  maybe (writeBS "Please specify echo parameter") writeBS slug


getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- getEnv "REDIS_HOST"
  database <- liftM read (getEnv "REDIS_DATABASE")
  password <- liftM pack (getEnv "REDIS_PASSWORD")
  return defaultConnectInfo {
    connectHost = host,
    connectDatabase = database,
    connectAuth = Just password
  }
