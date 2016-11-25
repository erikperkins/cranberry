{-# LANGUAGE OverloadedStrings #-}

module Api.Core where

import Api.Services.TodoService
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B
import Control.Lens

data Api = Api { _todoService :: Snaplet TodoService }
todoService :: Lens' Api (Snaplet TodoService)
todoService = lens _todoService (\a b -> a { _todoService = b })

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  todoSnaplet <- nestSnaplet "todo" todoService todoServiceInit
  addRoutes apiRoutes
  return $ Api todoSnaplet

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200
