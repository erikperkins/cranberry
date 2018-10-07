{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import qualified Data.Text as T (Text)
import Data.Aeson (ToJSON(toJSON), object, (.=))
import qualified Data.ByteString.Char8 as B (ByteString, readInt)

data TimeDatum = TimeDatum {
  time :: Int,
  datum :: Double
} deriving (Show)

instance ToJSON TimeDatum where
  toJSON (TimeDatum time datum) = object ["time" .= time, "datum" .= datum]

class ToTimeDatum a where toTimeDatum :: (Int, a) -> TimeDatum

instance ToTimeDatum Double where
  toTimeDatum (t, d) = TimeDatum{time = t, datum = d}

instance ToTimeDatum Int where
  toTimeDatum (t, d) = TimeDatum{time = t, datum = fromIntegral d}

data Forecast = Forecast {
  observed :: [TimeDatum],
  predicted :: [TimeDatum]
} deriving (Show)

instance ToJSON Forecast where
  toJSON (Forecast observed predicted) =
    object ["observed" .= observed, "predicted" .= predicted]

toForecast :: [TimeDatum] -> [TimeDatum] -> Forecast
toForecast o p = Forecast{observed = o, predicted = p}

readPair :: (B.ByteString, B.ByteString) -> (Int, Int)
readPair (k, v) = (byteToInt k, byteToInt v)

byteToInt :: B.ByteString -> Int
byteToInt byte = case (B.readInt byte) of
  Just (b, _) -> b
  Nothing -> 0
