{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ( mempty )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Network.WebSockets as WS
import Network.WebSockets ( WebSockets, Hybi00 )
import Data.ByteString ( ByteString )
import Server ( serve )

main :: IO ()
main = defaultMainWithOpts [testCase "expectedBehaviour" testExpectedBehaviour] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 15000000) }) }

testExpectedBehaviour :: Assertion
testExpectedBehaviour = do
    serve "localhost" 8765
    WS.connect "ws://localhost" 8765 "" testWS
  where
    testWS :: WebSockets Hybi00 ()
    testWS = do
        WS.sendTextData ("Hello" :: ByteString)
        return ()
