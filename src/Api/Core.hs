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

-- lensSetter :: (c -> d) -> a -> b -> (b -> a -> a)
-- lensSetter s a b = a { s = b }

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  ts <- nestSnaplet "todos" todoService todoServiceInit
  addRoutes apiRoutes
  return $ Api ts

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200
