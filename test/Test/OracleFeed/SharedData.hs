{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.OracleFeed.SharedData (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Scripts hiding (evaluateScript)
import Test.Utils
import OracleFeed

tests :: TestTree
tests = testGroup "SharedData Construction"
    [ testCase "Getters Validator" $ assertValidator testGetters
    , testCase "Setters Validator" $ assertValidator testSetters
    ]

{-# INLINABLE testPriceMapGet #-}
testPriceMapGet :: SharedData -> Bool
testPriceMapGet ex = traceIfFalse "Wrong Shared Data" $ takePM pm == exPriceMap'
  where
    pm = case getSharedPriceData ex of
        Nothing -> traceError "Did not find the shared PriceMap"
        Just a -> a

{-# INLINABLE gettersValidator #-}
gettersValidator :: ()
gettersValidator = if testPriceMapGet sharedData
        then ()
        else traceError "Tests Failed"
  where
    sharedData :: SharedData
    sharedData = case getSharedData (OracleFeed exBD) of
        Nothing -> traceError "Failed to find SharedData"
        Just bd -> bd

testGetters :: Script
testGetters = fromCompiledCode $$(compile [|| gettersValidator ||])

--------------------------------------------------------------------------------

{-# INLINABLE testSetPriceMap #-}
testSetPriceMap :: SharedData -> Bool
testSetPriceMap ex = traceIfFalse "Wrong Timestamp Set" $ testPriceMapGet sharedData
  where
    sharedData = setSharedPriceData (PriceMap exPriceMap') ex


{-# INLINABLE settersValidator #-}
settersValidator :: ()
settersValidator = if testSetPriceMap sharedData
    then ()
    else traceError "Setter Tests Failed"
  where
    sharedData :: SharedData
    sharedData = setSharedPriceData (setTimestamp 5555 emptySharedPriceMap) emptySharedData

testSetters :: Script
testSetters = fromCompiledCode $$(compile [|| settersValidator ||])