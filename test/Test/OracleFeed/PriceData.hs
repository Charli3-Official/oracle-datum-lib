{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.OracleFeed.PriceData (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Scripts hiding (evaluateScript)
import Test.Utils
import OracleFeed

tests :: TestTree
tests = testGroup "PriceData Construction"
    [ testCase "Getters Validator" $ assertValidator testGetters
    , testCase "Setters Validator" $ assertValidator testSetters
    ]

{-# INLINABLE testPriceMapGet #-}
testPriceMapGet :: PriceMap -> Bool
testPriceMapGet ex = traceIfFalse "Wrong Price Map" $ ex == exPriceMap

{-# INLINABLE testPriceGet #-}
testPriceGet :: PriceMap -> Bool
testPriceGet ex = traceIfFalse "Incorrect Price Fetched" $
                    getPrice ex == exPrice

{-# INLINABLE testTimestampGet #-}
testTimestampGet :: PriceMap -> Bool
testTimestampGet ex = case getTimestamp ex of
                        Just a -> traceIfFalse "Incorrect Timestampt Fetched" $
                                a == exTimestamp
                        Nothing -> traceError "No Timestamp Found"

{-# INLINABLE testExpiryGet #-}
testExpiryGet :: PriceMap -> Bool
testExpiryGet ex = case getExpiry ex of
                        Just pt -> traceIfFalse "Incorrect Expiry Fetched" $
                                pt == exExpiry
                        Nothing -> traceError "No Expiry Found"

{-# INLINABLE testPrecisionGet #-}
testPrecisionGet :: PriceMap -> Bool
testPrecisionGet ex = case getPrecision ex of
                        Just pt -> traceIfFalse "Incorrect Precision Fetched" $
                                pt == exPrecision
                        Nothing -> traceError "No Precision Found"

{-# INLINABLE testBaseIdGet #-}
testBaseIdGet :: PriceMap -> Bool
testBaseIdGet ex = case getBaseId ex of
                        Just a -> traceIfFalse "Incorrect BaseId Fetched" $
                                a == exBaseId
                        Nothing -> traceError "No Base Id Found"

{-# INLINABLE testQuoteIdGet #-}
testQuoteIdGet :: PriceMap -> Bool
testQuoteIdGet ex = case getQuoteId ex of
                        Just a -> traceIfFalse "Incorrect QuoteId Fetched" $
                                a == exQuoteId
                        Nothing -> traceError "No Quote Id Found"

{-# INLINABLE testBaseSymbolGet #-}
testBaseSymbolGet :: PriceMap -> Bool
testBaseSymbolGet ex = case getBaseSymbol ex of
                        Just a -> traceIfFalse "Incorrect Base Symbol Fetched" $
                                a == exBaseSymbol
                        Nothing -> traceError "No Base Symbol Found"

{-# INLINABLE testBaseNameGet #-}
testBaseNameGet :: PriceMap -> Bool
testBaseNameGet ex = case getBaseName ex of
                        Just a -> traceIfFalse "Incorrect Base Name Fetched" $
                                a == exBaseName
                        Nothing -> traceError "No Base Name Found"

{-# INLINABLE testQuoteSymbolGet #-}
testQuoteSymbolGet :: PriceMap -> Bool
testQuoteSymbolGet ex = case getQuoteSymbol ex of
                        Just a -> traceIfFalse "Incorrect Quote Symbol Fetched" $
                                a == exQuoteSymbol
                        Nothing -> traceError "No Quote Symbol Found"

{-# INLINABLE testQuoteNameGet #-}
testQuoteNameGet :: PriceMap -> Bool
testQuoteNameGet ex = case getQuoteName ex of
                        Just a -> traceIfFalse "Incorrect Quote Name Fetched" $
                                a == exQuoteName
                        Nothing -> traceError "No Quote Name Found"

{-# INLINABLE testCustomFieldGet #-}
testCustomFieldGet :: PriceMap -> Bool
testCustomFieldGet ex = case getPriceCustomField @Integer ex exCustomIdx of
                            Just a -> traceIfFalse "Incorrect Custom Field Fetched" $
                                        a == exCustomField
                            Nothing -> traceError "No Custom Field Found"

{-# INLINABLE priceMapTestsGetters #-}
priceMapTestsGetters :: [PriceMap -> Bool]
priceMapTestsGetters = [ testPriceMapGet
                       , testPriceGet
                       , testTimestampGet
                       , testExpiryGet
                       , testBaseIdGet
                       , testQuoteIdGet
                       , testBaseSymbolGet
                       , testBaseNameGet
                       , testQuoteSymbolGet
                       , testQuoteNameGet
                       , testCustomFieldGet
                       ]

{-# INLINABLE gettersValidator #-}
gettersValidator :: ()
gettersValidator = if and results then () else traceError "Tests Failed"
  where
    results :: [Bool]
    results = map ($ priceMap) priceMapTestsGetters

    priceMap :: PriceMap
    priceMap = getPriceMap $ head $ getPriceDatas exBD

testGetters :: Script
testGetters = fromCompiledCode $$(compile [|| gettersValidator ||])

--------------------------------------------------------------------------------

{-# INLINABLE testTimestampSet #-}
testTimestampSet :: PriceMap -> Bool
testTimestampSet ex = traceIfFalse "Wrong Timestamp Set" $ testTimestampGet priceMap
  where
    priceMap = setTimestamp ex exTimestamp

{-# INLINABLE testExpirySet #-}
testExpirySet :: PriceMap -> Bool
testExpirySet ex = traceIfFalse "Wrong Expiry Set" $ testExpiryGet priceMap
  where
    priceMap = setExpiry ex exExpiry

{-# INLINABLE testPrecisionSet #-}
testPrecisionSet :: PriceMap -> Bool
testPrecisionSet ex = traceIfFalse "Wrong Precision Set" $ testPrecisionGet priceMap
  where
    priceMap = setPrecision ex exPrecision

{-# INLINABLE testBaseIdSet #-}
testBaseIdSet :: PriceMap -> Bool
testBaseIdSet ex = traceIfFalse "Wrong BaseId Set" $ testBaseIdGet priceMap
  where
    priceMap = setBaseId ex exBaseId

{-# INLINABLE testQuoteIdSet #-}
testQuoteIdSet :: PriceMap -> Bool
testQuoteIdSet ex = traceIfFalse "Wrong QuoteId Set" $ testQuoteIdGet priceMap
  where
    priceMap = setQuoteId ex exQuoteId

{-# INLINABLE testBaseSymbolSet #-}
testBaseSymbolSet :: PriceMap -> Bool
testBaseSymbolSet ex = traceIfFalse "Wrong BaseSymbol Set" $ testBaseSymbolGet priceMap
  where
    priceMap = setBaseSymbol ex exBaseSymbol

{-# INLINABLE testQuoteSymbolSet #-}
testQuoteSymbolSet :: PriceMap -> Bool
testQuoteSymbolSet ex = traceIfFalse "Wrong QuoteSymbol Set" $ testQuoteSymbolGet priceMap
  where
    priceMap = setQuoteSymbol ex exQuoteSymbol

{-# INLINABLE testBaseNameSet #-}
testBaseNameSet :: PriceMap -> Bool
testBaseNameSet ex = traceIfFalse "Wrong BaseName Set" $ testBaseNameGet priceMap
  where
    priceMap = setBaseName ex exBaseName

{-# INLINABLE testQuoteNameSet #-}
testQuoteNameSet :: PriceMap -> Bool
testQuoteNameSet ex = traceIfFalse "Wrong QuoteName Set" $ testQuoteNameGet priceMap
  where
    priceMap = setQuoteName ex exQuoteName

{-# INLINABLE testCustomFieldSet #-}
testCustomFieldSet :: PriceMap -> Bool
testCustomFieldSet ex = traceIfFalse "Wrong CustomField Set" $ testCustomFieldGet priceMap
  where
    priceMap = setPriceCustomField ex exCustomIdx exCustomField

{-# INLINABLE priceMapTestsSetters #-}
priceMapTestsSetters :: [PriceMap -> Bool]
priceMapTestsSetters = [ testTimestampSet
                       , testExpirySet
                       , testBaseIdSet
                       , testQuoteIdSet
                       , testBaseSymbolSet
                       , testBaseNameSet
                       , testQuoteSymbolSet
                       , testQuoteNameSet
                       , testCustomFieldSet
                       ]

{-# INLINABLE settersValidator #-}
settersValidator :: ()
settersValidator = if and results then () else traceError "Setter Tests Failed"
  where
    results :: [Bool]
    results = map ($ priceMap) priceMapTestsSetters

    priceMap :: PriceMap
    priceMap = mkPriceMap exPrice

testSetters :: Script
testSetters = fromCompiledCode $$(compile [|| settersValidator ||])