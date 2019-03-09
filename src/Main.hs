{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Twitter
import Control.Concurrent (forkIO)
import Site
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet (serveSnaplet)


main :: IO ()
main = do
  _ <- forkIO twitterStream
  serveSnaplet defaultConfig cranberry
  return ()
