{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.ForecastService where

import Api.Types
import Control.Lens (lens, Lens')
import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.Aeson (encode, Object, Value(..))
import Data.List (sort, unzip, zip)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.Redis (ConnectInfo, connectDatabase, connectHost, defaultConnectInfo)
import Database.Redis (get, hget, hgetall, keys, set)
import qualified Data.ByteString.Char8 as B (ByteString)
import GHC.Exts (fromList)
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
  modifyResponse $ setHeader "Content-Type" "application/json"
  keys <- runRedisDB redis $ do keys "en:[0-9]*"
  key:_ <- case keys of
    Left _ -> return []
    Right ks -> return . reverse . sort $ ks
  values <- runRedisDB redis $ do hgetall key
  series <- case values of
    Left _ -> return [(0, 0)]
    Right vs -> return . (map readPair) $ vs
  predicted <- return . (map toTimeDatum) . (forecast 0.5) $ series
  observed <- return . (map toTimeDatum) $ series
  writeLBS . encode $ toForecast observed predicted

forecast :: Double -> [(Int, Int)] -> [(Int, Double)]
forecast a s = do
  (t:ts', vs) <- return . unzip . init . sort $ s
  fs <- return $ scanl (\x y -> (1.0 - a) * x + a * y) 20 $ map fromIntegral vs
  ts <- return $ scanl (\x y -> y + 60) t (t:ts')
  zip ts fs

connection :: ConnectInfo
connection = defaultConnectInfo {
  connectHost = "storage.datapun.net",
  connectDatabase = 3
}
