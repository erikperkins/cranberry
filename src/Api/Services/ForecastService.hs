{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.ForecastService where

import Api.Types
import Control.Lens (lens, Lens')
import Control.Monad.IO.Class
import Data.Aeson (encode)
import Data.Text.Encoding (decodeUtf8)
import Database.Redis (ConnectInfo, connectDatabase, connectHost, defaultConnectInfo, get, set)
import qualified Data.ByteString.Char8 as B (ByteString)
import Snap.Core (method, Method(GET), modifyResponse, setHeader, setResponseCode, writeLBS)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, nestSnaplet, Snaplet, SnapletInit)
import Snap.Snaplet.RedisDB

data ForecastService = ForecastService { _redis :: Snaplet RedisDB }
redis :: Lens' ForecastService (Snaplet RedisDB)
redis = lens _redis (\a b -> a { _redis = b })

forecastServiceInit :: SnapletInit b ForecastService
forecastServiceInit = makeSnaplet "forecast" "Forecast Service" Nothing $ do
  redis <- nestSnaplet "redis" redis $ redisDBInit connection
  addRoutes forecastRoutes
  return $ ForecastService redis

forecastRoutes :: [(B.ByteString, Handler b ForecastService ())]
forecastRoutes = [("/", method GET forecastEndpoint)]

forecastEndpoint :: Handler b ForecastService ()
forecastEndpoint = do
  result <- runRedisDB redis $ do get "foo"
  value <- case result of
    Right (Just reply) -> return $ decodeUtf8 reply
    Left _ -> return $ decodeUtf8 ""
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ value

connection :: ConnectInfo
connection = defaultConnectInfo {
  connectHost = "storage.datapun.net",
  connectDatabase = 2
}
