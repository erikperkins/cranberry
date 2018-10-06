{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import qualified Data.Text as T (Text)
import Data.Aeson (ToJSON(toJSON), object, (.=))

data Forecast = Forecast {
  observed :: T.Text,
  predicted :: T.Text
}

instance ToJSON Forecast where
  toJSON (Forecast predicted observed) =
    object ["predicted" .= predicted, "observed" .= observed]
