{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import Data.Aeson (ToJSON(toJSON), object, (.=))

import qualified Data.ByteString.Char8 as B (ByteString, readInt)


data TimeDatum = TimeDatum {
  time :: Int,
  datum :: Double
} deriving (Show)


instance ToJSON TimeDatum where
  toJSON (TimeDatum t d) = object ["time" .= t, "datum" .= d]


class ToTimeDatum a where toTimeDatum :: (Int, a) -> TimeDatum


instance ToTimeDatum Double where
  toTimeDatum (t, d) = TimeDatum{time = t, datum = d}


instance ToTimeDatum Int where
  toTimeDatum (t, d) = TimeDatum{time = t, datum = fromIntegral d}


data ForecastData = ForecastData {
  observed :: [TimeDatum],
  predicted :: [TimeDatum]
} deriving (Show)


instance ToJSON ForecastData where
  toJSON (ForecastData o p) =
    object ["observed" .= o, "predicted" .= p]


toForecast :: [TimeDatum] -> [TimeDatum] -> ForecastData
toForecast o p = ForecastData{observed = o, predicted = p}


readPair :: (B.ByteString, B.ByteString) -> (Int, Int)
readPair (k, v) = (byteToInt k, byteToInt v)


byteToInt :: B.ByteString -> Int
byteToInt byte = case (B.readInt byte) of
  Just (b, _) -> b
  Nothing -> 0
