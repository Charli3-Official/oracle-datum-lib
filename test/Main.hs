module Main (main) where

import Test.Tasty ( defaultMain, testGroup )
import qualified Test.OracleFeed.PriceData

main :: IO ()
main = defaultMain . testGroup "Oracle Feed Tests" $
    [ Test.OracleFeed.PriceData.tests
    ]