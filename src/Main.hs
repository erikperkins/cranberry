{-# LANGUAGE OverloadedStrings #-}
-- serve site

module Main where

import Site
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet (serveSnaplet)

main :: IO ()
-- Add config here from environment variables
main = serveSnaplet defaultConfig cranberry
