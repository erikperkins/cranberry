{-# LANGUAGE OverloadedStrings #-}

module Site (app) where

import Application
import Api.Core (Api(Api), apiInit)
import Snap.Core
import Snap.Snaplet
import Data.ByteString
import Data.Text as T
import Control.Applicative

routes :: [(ByteString, Handler App App ())]
routes = []

app :: SnapletInit App App
app = makeSnaplet "app" "An example snaplet application" Nothing $ do
  api <- nestSnaplet "api" api apiInit
  addRoutes routes
  return $ App api
