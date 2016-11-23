module Main where

import Snap.Http.Server
import Snap.Snaplet (serveSnaplet)
import Snap.Snaplet.Config
import Site

main :: IO ()
main = serveSnaplet defaultConfig app
