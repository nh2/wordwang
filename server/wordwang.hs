module Main where

import           Network.WebSockets (runServer, WebSockets, Hybi00, Request)

import           Server

main :: IO ()
main = runServer "0.0.0.0" 8888 (server :: Request -> WebSockets Hybi00 ())

