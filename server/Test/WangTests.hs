module Main where

import Data.Monoid ( mempty )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts [testCase "expectedBehaviour" testExpectedBehaviour] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 15000000) }) }

testExpectedBehaviour :: Assertion
testExpectedBehaviour = do
    return ()
