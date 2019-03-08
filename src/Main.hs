{-# LANGUAGE OverloadedStrings #-}
-- Serve site

module Main where

import Site
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet (serveSnaplet)
import Control.Concurrent (forkIO)
import Api.Twitter

main :: IO ()
main = do
  _ <- forkIO twitterStream
  serveSnaplet defaultConfig cranberry
  return ()
