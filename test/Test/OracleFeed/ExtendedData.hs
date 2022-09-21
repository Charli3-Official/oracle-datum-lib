{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.OracleFeed.ExtendedData (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Scripts hiding (evaluateScript)
import Test.Utils
import OracleFeed

tests :: TestTree
tests = testGroup "ExtendedData Construction"
    [ testCase "Getters Validator" $ assertValidator testGetters
    , testCase "Setters Validator" $ assertValidator testSetters
    ]

{-# INLINABLE testExtendedDataGet #-}
testExtendedDataGet :: ExtendedData -> Bool
testExtendedDataGet ex = traceIfFalse "Wrong Extended Data" $ ex == exExtData

{-# INLINABLE testOracleProviderGet #-}
testOracleProviderGet :: ExtendedData -> Bool
testOracleProviderGet ex = case getOracleProvider ex of
                        Just a -> traceIfFalse "Incorrect Oracle Provider Fetched" $
                                a == exOracleProviderId
                        Nothing -> traceError "No Oracle Provider Found"

{-# INLINABLE testDataSignatoriesCountGet #-}
testDataSignatoriesCountGet :: ExtendedData -> Bool
testDataSignatoriesCountGet ex = case getDataSignatoriesCount ex of
                        Just a -> traceIfFalse "Incorrect Data Signatories Count Fetched" $
                                a == exDataSignatoriesCount
                        Nothing -> traceError "No Data Signatories Count Found"

{-# INLINABLE testDataSourceCountGet #-}
testDataSourceCountGet :: ExtendedData -> Bool
testDataSourceCountGet ex = case getDataSourceCount ex of
                        Just a -> traceIfFalse "Incorrect Data Source Count Fetched" $
                                a == exDataSourceCount
                        Nothing -> traceError "No Data Source Count Found"

{-# INLINABLE testOracleProviderSignatureGet #-}
testOracleProviderSignatureGet :: ExtendedData -> Bool
testOracleProviderSignatureGet ex = case getOracleProviderSignature ex of
                        Just a -> traceIfFalse "Incorrect Oracle Provider Signature Fetched" $
                                a == exOracleProviderSignature
                        Nothing -> traceError "No Oracle Provider Signature Found"

{-# INLINABLE extendedDataTestsGetters #-}
extendedDataTestsGetters :: [PriceMap -> Bool]
extendedDataTestsGetters = [ testExtendedDataGet
                           , testOracleProviderGet
                           , testDataSignatoriesCountGet
                           , testDataSourceCountGet
                           , testOracleProviderSignatureGet
                           ]

{-# INLINABLE gettersValidator #-}
gettersValidator :: ()
gettersValidator = if and results then () else traceError "Tests Failed"
  where
    results :: [Bool]
    results = map ($ extendedData) extendedDataTestsGetters

    extendedData :: ExtendedData
    extendedData = case getExtendedData exBD of
      Nothing -> traceError "Extended Data not found"
      Just bd -> bd


testGetters :: Script
testGetters = fromCompiledCode $$(compile [|| gettersValidator ||])

--------------------------------------------------------------------------------

{-# INLINABLE testOracleProviderSet #-}
testOracleProviderSet :: ExtendedData -> Bool
testOracleProviderSet ex = traceIfFalse "Wrong OracleProvider Set" $ testOracleProviderGet extendedData
  where
    extendedData = setOracleProvider ex exOracleProviderId

{-# INLINABLE testDataSourceCountSet #-}
testDataSourceCountSet :: ExtendedData -> Bool
testDataSourceCountSet ex = traceIfFalse "Wrong DataSourceCount Set" $ testDataSourceCountGet extendedData
  where
    extendedData = setDataSourceCount ex exDataSourceCount

{-# INLINABLE testDataSignatoriesCountSet #-}
testDataSignatoriesCountSet :: ExtendedData -> Bool
testDataSignatoriesCountSet ex = traceIfFalse "Wrong DataSignatoriesCount Set" $ testDataSignatoriesCountGet extendedData
  where
    extendedData = setDataSignatoriesCount ex exDataSignatoriesCount

{-# INLINABLE testOracleProviderSignatureSet #-}
testOracleProviderSignatureSet :: ExtendedData -> Bool
testOracleProviderSignatureSet ex = traceIfFalse "Wrong OracleProviderSignature Set" $ testOracleProviderSignatureGet extendedData
  where
    extendedData = setOracleProviderSignature ex exOracleProviderSignature

{-# INLINABLE extendedDataTestsSetters #-}
extendedDataTestsSetters :: [ExtendedData -> Bool]
extendedDataTestsSetters = [ testOracleProviderSet
                           , testDataSourceCountSet
                           , testDataSignatoriesCountSet
                           , testOracleProviderSignatureSet
                           ]

{-# INLINABLE settersValidator #-}
settersValidator :: ()
settersValidator = if and results then () else traceError "Setter Tests Failed"
  where
    results :: [Bool]
    results = map ($ extendedData) extendedDataTestsSetters

    extendedData :: ExtendedData
    extendedData = emptyExtendedData

testSetters :: Script
testSetters = fromCompiledCode $$(compile [|| settersValidator ||])