{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Forecast where

import Api.Types (readPair, toForecast, toTimeDatum)
import Control.Lens (lens, Lens')
import Data.Aeson (encode)
import Data.List (sort, unzip, zip)
import Database.Redis (hgetall, keys)
import qualified Data.ByteString.Char8 as B (ByteString)
import Snap.Core (method, Method(GET), modifyResponse, setHeader, writeLBS)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, Snaplet, SnapletInit)
import Snap.Snaplet.RedisDB (RedisDB, runRedisDB)

data Forecast = Forecast { _redis :: Snaplet RedisDB }
redis :: Lens' Forecast (Snaplet RedisDB)
redis = lens _redis (\a b -> a { _redis = b })

forecastInit :: Snaplet RedisDB -> SnapletInit b Forecast
forecastInit redisSnaplet = makeSnaplet "forecast" "Forecast" Nothing $ do
  addRoutes forecastRoutes
  return $ Forecast redisSnaplet

forecastRoutes :: [(B.ByteString, Handler b Forecast ())]
forecastRoutes = [("/", method GET forecastEndpoint)]

forecastEndpoint :: Handler b Forecast ()
forecastEndpoint = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  redisKeys <- runRedisDB redis $ keys "tweet:[0-9]*"
  key:_ <- case redisKeys of
    Left _ -> return []
    Right ks -> return . reverse . sort $ ks
  values <- runRedisDB redis $ hgetall key
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
