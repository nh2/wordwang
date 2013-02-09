module Main where

import           Network.WebSockets (runServer, WebSockets, Hybi00, Request)

import           Server

main :: IO ()
main = serve "0.0.0.0" 8888
