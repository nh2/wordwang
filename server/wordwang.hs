module Main where

import           Server

main :: IO ()
main = serve "0.0.0.0" 8888
