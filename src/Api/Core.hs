{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Api.Services.ForecastService
import Control.Lens (lens, Lens')
import qualified Data.ByteString.Char8 as B (ByteString)
import Snap.Core (method, Method(GET), modifyResponse, setResponseCode)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, nestSnaplet, Snaplet, SnapletInit)

data Api = Api { _forecastService :: Snaplet ForecastService }
forecastService :: Lens' Api (Snaplet ForecastService)
forecastService = lens _forecastService (\a b -> a { _forecastService = b })

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  forecastSnaplet <- nestSnaplet "forecast" forecastService forecastServiceInit
  addRoutes apiRoutes
  return $ Api forecastSnaplet

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200
