{-# LANGUAGE OverloadedStrings #-}
-- Define routes and request handlers. Export via app function.

module Site (cranberry) where

import Application
import Data.ByteString (ByteString)
import Snap.Core (writeBS, getParam)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, nestSnaplet, SnapletInit)
import Api.Forecast (forecastInit)
import Snap.Snaplet.RedisDB
import Database.Redis (ConnectInfo, defaultConnectInfo)
import Database.Redis (connectDatabase, connectHost, connectAuth)

cranberry :: SnapletInit App App
cranberry = makeSnaplet "app" "Snaplet example application" Nothing $ do
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

connection :: ConnectInfo
connection = defaultConnectInfo {
  connectHost = "storage.datapun.net",
  connectDatabase = 3,
  connectAuth = Just "redis"
}
