module Main where

import Snap.Http.Server
import Snap.Snaplet (serveSnaplet)
import Site

main :: IO ()
main = serveSnaplet defaultConfig app
