{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.ForecastService where

import Api.Types
import Control.Lens (lens, Lens')
import Data.Aeson (encode)
import Data.List (sort, unzip, zip)
import Database.Redis (ConnectInfo, defaultConnectInfo)
import Database.Redis (connectDatabase, connectHost, connectAuth)
import Database.Redis (hgetall, keys)
import qualified Data.ByteString.Char8 as B (ByteString)
import Snap.Core (method, Method(GET), modifyResponse, setHeader, writeLBS)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, nestSnaplet, Snaplet, SnapletInit)
import Snap.Snaplet.RedisDB

data ForecastService = ForecastService { _redis :: Snaplet RedisDB }
redis :: Lens' ForecastService (Snaplet RedisDB)
redis = lens _redis (\a b -> a { _redis = b })

forecastServiceInit :: SnapletInit b ForecastService
forecastServiceInit = makeSnaplet "forecast" "Forecast Service" Nothing $ do
  redisSnaplet <- nestSnaplet "redis" redis $ redisDBInit connection
  addRoutes forecastRoutes
  return $ ForecastService redisSnaplet

forecastRoutes :: [(B.ByteString, Handler b ForecastService ())]
forecastRoutes = [("/", method GET forecastEndpoint)]

forecastEndpoint :: Handler b ForecastService ()
forecastEndpoint = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  redisKeys <- runRedisDB redis $ do keys "en:[0-9]*"
  key:_ <- case redisKeys of
    Left _ -> return []
    Right ks -> return . reverse . sort $ ks
  values <- runRedisDB redis $ do hgetall key
  series <- case values of
    Left _ -> return [(0, 0)]
    Right vs -> return . (map readPair) $ vs
  predictedValues <- return . (map toTimeDatum) . (forecast 0.5) $ series
  observedValues <- return . (map toTimeDatum) $ series
  writeLBS . encode $ toForecast observedValues predictedValues

forecast :: Double -> [(Int, Int)] -> [(Int, Double)]
forecast a s = do
  (t:ts', vs) <- return . unzip . init . sort $ s
  fs <- return $ scanl (\x y -> (1.0 - a) * x + a * y) 20 $ map fromIntegral vs
  ts <- return $ scanl (\_ y -> y + 60) t (t:ts')
  zip ts fs

connection :: ConnectInfo
connection = defaultConnectInfo {
  connectHost = "storage.datapun.net",
  connectDatabase = 3,
  connectAuth = Just "redis"
}
