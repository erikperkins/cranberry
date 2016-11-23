{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.TodoService where

import Api.Types (Todo(Todo)) -- import setter
import Control.Lens
import Control.Monad.State.Class (get)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

data TodoService = TodoService { _postg :: Snaplet Postgres }
postg :: Lens' TodoService (Snaplet Postgres)
postg = lens _postg (\a b -> a { _postg = b })

todoServiceInit :: SnapletInit b TodoService
todoServiceInit = makeSnaplet "todos" "Todo Service" Nothing $ do
  pg <- nestSnaplet "pg" postg pgsInit
  addRoutes todoRoutes
  return $ TodoService pg

todoRoutes :: [(B.ByteString, Handler b TodoService ())]
todoRoutes = [
    ("/", method GET getTodos),
    ("/", method POST createTodo)
  ]

getTodos :: Handler b TodoService ()
getTodos = do
  todos <- query_ "select * from todos"
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (todos :: [Todo])

createTodo :: Handler b TodoService ()
createTodo = do
  text <- getPostParam "text"
  todo <- execute "insert into todos (text) values (?)" (Only text)
  modifyResponse $ setResponseCode 201

instance HasPostgres (Handler b TodoService) where
  getPostgresState = with postg get
