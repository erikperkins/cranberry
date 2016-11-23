{-# LANGUAGE OverloadedStrings #-}

module Site (app) where

import Snap.Snaplet
import Data.ByteString
import Api.Core (apiInit)
import Application

routes :: [(ByteString, Handler App App ())]
routes = []

app :: SnapletInit App App
app = makeSnaplet "app" "An example snaplet application" Nothing $ do
  apiSnaplet <- nestSnaplet "api" api apiInit
  addRoutes routes
  return $ App apiSnaplet
