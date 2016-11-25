{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.TodoService where

import Api.Types (Todo(Todo))
import Control.Lens
import Control.Monad.State.Class (get)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

data TodoService = TodoService { _db :: Snaplet Postgres }
db :: Lens' TodoService (Snaplet Postgres)
db = lens _db (\a b -> a { _db = b })

instance HasPostgres (Handler b TodoService) where
  getPostgresState = with db get

todoServiceInit :: SnapletInit b TodoService
todoServiceInit = makeSnaplet "todo" "Todo Service" Nothing $ do
  pg <- nestSnaplet "pg" db pgsInit
  addRoutes todoRoutes
  return $ TodoService pg

todoRoutes :: [(B.ByteString, Handler b TodoService ())]
todoRoutes = [
    ("/", method GET indexTodos),
    ("/:id", method GET showTodo),
    ("/new", method GET newTodo),
    ("/", method POST createTodo),
    ("/:id/edit", method GET editTodo),
    ("/:id", method PATCH updateTodo),
    ("/:id", method DELETE destroyTodo)
  ]

indexTodos :: Handler b TodoService ()
indexTodos = do
  todos <- query_ "select * from todos"
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (todos :: [Todo])

showTodo :: Handler b TodoService ()
showTodo = do
  Just id <- getParam "id"
  todo:[] <- query "select * from todos where id = ?" (Only id)
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (todo :: Todo)

newTodo :: Handler b TodoService ()
newTodo = do return ()

createTodo :: Handler b TodoService ()
createTodo = do
  text <- getPostParam "text"
  todo <- execute "insert into todos (text) values (?)" (Only text)
  modifyResponse $ setResponseCode 201

editTodo :: Handler b TodoService ()
editTodo = do return ()

updateTodo :: Handler b TodoService ()
updateTodo = do
  id <- getParam "id"
  text <- getPostParam "text"
  todo <- execute "update todos set text = ? where id = ?" (text, id)
  modifyResponse $ setResponseCode 201

destroyTodo :: Handler b TodoService ()
destroyTodo = do
  id <- getParam "id"
  todo <- execute "delete from todos where id = ?" (Only id)
  modifyResponse $ setResponseCode 201
