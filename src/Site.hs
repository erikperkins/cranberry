{-# LANGUAGE OverloadedStrings #-}
-- Define routes and request handlers. Export via app function.

module Site (cranberry) where

import Application
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Snap.Core (writeBS, getParam)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, nestSnaplet, SnapletInit)
import Api.Forecast (forecastInit)
import Snap.Snaplet.RedisDB
import Database.Redis (ConnectInfo, defaultConnectInfo)
import Database.Redis (connectDatabase, connectHost, connectAuth)

import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)

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
  database <- getEnv "REDIS_DATABASE"
  password <- getEnv "REDIS_PASSWORD"
  return defaultConnectInfo {
    connectHost = host,
    connectDatabase = read database,
    connectAuth = Just (pack password)
  }
