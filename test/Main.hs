module Main (main) where

import Test.Tasty ( defaultMain, testGroup )
import qualified Test.OracleFeed.PriceData
import qualified Test.OracleFeed.SharedData
import qualified Test.OracleFeed.ExtendedData

main :: IO ()
main = defaultMain . testGroup "Oracle Feed Tests" $
    [ Test.OracleFeed.PriceData.tests
    , Test.OracleFeed.SharedData.tests
    , Test.OracleFeed.ExtendedData.tests
    ]