{-# LANGUAGE OverloadedStrings #-}

module Site (app) where

import Snap.Core (writeBS, getParam)
import Snap.Snaplet
import Data.ByteString
import Api.Core (apiInit)
import Application

app :: SnapletInit App App
app = makeSnaplet "app" "An example snaplet application" Nothing $ do
  apiSnaplet <- nestSnaplet "api" api apiInit
  addRoutes routes
  return $ App apiSnaplet

routes :: [(ByteString, AppHandler ())]
routes = [
    ("/", root),
    ("echo/:param", echo)
  ]

root :: AppHandler ()
root = writeBS "Welcome to Snap!\n"

echo :: AppHandler ()
echo = do
  param <- getParam "param"
  maybe (writeBS "Please specify echo parameter") writeBS param
