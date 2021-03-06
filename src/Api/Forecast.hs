{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Forecast where

import Api.Types (readPair, toForecast, toTimeDatum)
import Control.Lens (lens, Lens')
import Data.Aeson (encode)
import Data.List (sort, unzip, zip)
import Database.Redis (hgetall, keys)
import Snap.Core (method, Method(GET), modifyResponse, setHeader, writeLBS)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, Snaplet, SnapletInit)
import Snap.Snaplet.RedisDB (RedisDB, runRedisDB)

import qualified Data.ByteString.Char8 as B (ByteString)


data Forecast = Forecast { _redis :: Snaplet RedisDB }
redis :: Lens' Forecast (Snaplet RedisDB)
redis = lens _redis (\a b -> a { _redis = b })

forecast :: Double -> [(Int, Int)] -> [(Int, Double)]
forecast a s = do
  let (t:ts', vs) = unzip . init . sort $ s
  let fs = scanl (\x y -> (1.0 - a) * x + a * y) 20 $ map fromIntegral vs
  let ts = scanl (\_ y -> y + 60) t (t:ts')
  zip ts fs

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

  let predictedValues = (map toTimeDatum) . (forecast 0.5) $ series
  let observedValues = (map toTimeDatum) $ series
  writeLBS . encode $ toForecast observedValues predictedValues
