module Main (main) where

import Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain . testGroup "Oracle Feed Tests" $ []