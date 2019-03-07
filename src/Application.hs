-- Define application state type and alias for handler monad
module Application where

import Control.Lens (lens, Lens')
import Snap.Snaplet (Handler, Snaplet)
import Api.Services.ForecastService (ForecastService)
import Snap.Snaplet.RedisDB

data App = App {
  _forecast :: Snaplet ForecastService,
  _redis :: Snaplet RedisDB
}

forecast :: Lens' App (Snaplet ForecastService)
forecast = lens _forecast (\a b -> a { _forecast = b })

redis :: Lens' App (Snaplet RedisDB)
redis = lens _redis (\a b -> a { _redis = b })

type AppHandler = Handler App App
