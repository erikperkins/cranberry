{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Site
import Stream
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet (serveSnaplet)


main :: IO ()
main = do
  thread <- forkIO receive
  print thread
  serveSnaplet defaultConfig cranberry
  return ()
