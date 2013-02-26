{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent ( threadDelay )
import Data.ByteString ( ByteString )
import Data.Monoid ( mempty )
import Network.WebSockets ( WebSockets, Hybi00 )
import Server ( serve )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Network.WebSockets as WS

main :: IO ()
main = defaultMainWithOpts [testCase "expectedBehaviour" testExpectedBehaviour] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 15000000) }) }

testExpectedBehaviour :: Assertion
testExpectedBehaviour = do
    serve "127.0.0.1" 8765
    threadDelay 2000000
    WS.connect "127.0.0.1" 8765 "/" testWS
  where
    testWS :: WebSockets Hybi00 ()
    testWS = do
        WS.sendTextData ("Hello" :: ByteString)
        return ()
