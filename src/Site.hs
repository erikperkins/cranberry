{-# LANGUAGE OverloadedStrings #-}
-- Define routes and request handlers. Export via app function.

module Site (cranberry) where

import Api.Core (apiInit)
import Application
import Data.ByteString (ByteString)
import Snap.Core (writeBS, getParam)
import Snap.Snaplet (addRoutes, Handler, makeSnaplet, nestSnaplet, SnapletInit)

cranberry :: SnapletInit App App
cranberry = makeSnaplet "app" "Snaplet example application" Nothing $ do
  api <- nestSnaplet "api" api apiInit
  addRoutes routes
  return $ App api

routes :: [(ByteString, Handler App App ())]
routes = [
    ("/", root),
    ("echo/:slug", echo)
  ]

root :: AppHandler ()
root = writeBS "Hello, Snap!\n"

echo :: AppHandler ()
echo = do
  slug <- getParam "slug"
  maybe (writeBS "Please specify echo parameter") writeBS slug
