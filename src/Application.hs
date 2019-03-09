module Application where

import Api.Forecast (Forecast)
import Control.Lens (lens, Lens')
import Snap.Snaplet (Handler, Snaplet)
import Snap.Snaplet.RedisDB


data App = App {
  _forecast :: Snaplet Forecast,
  _redis :: Snaplet RedisDB
}


forecast :: Lens' App (Snaplet Forecast)
forecast = lens _forecast (\a b -> a { _forecast = b })


redis :: Lens' App (Snaplet RedisDB)
redis = lens _redis (\a b -> a { _redis = b })


type AppHandler = Handler App App
