module Main where

import           Network.WebSockets (runServer, WebSockets, Hybi00)

import           Server

main :: IO ()
main = runServer "0.0.0.0" 8888 (const (echo :: WebSockets Hybi00 ()))

