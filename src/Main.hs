{-# LANGUAGE OverloadedStrings #-}
-- serve site

module Main where

import Site
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet (serveSnaplet)

main :: IO ()
main = serveSnaplet defaultConfig cranberry
