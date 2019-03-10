{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Site
import Stream
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet (serveSnaplet)


main :: IO ()
main = do
  _ <- forkIO receive
  serveSnaplet defaultConfig cranberry
  return ()
