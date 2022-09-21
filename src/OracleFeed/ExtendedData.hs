module OracleFeed.ExtendedData where

import OracleFeed.Types
import PlutusTx.Builtins
import Utils ( insertConstrSMap, lookupConstrSMap, failIfNotStandard )
import Plutus.V1.Ledger.Api (ToData(toBuiltinData))
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api (FromData)

{-# INLINABLE emptyExtendedData #-}
emptyExtendedData :: ExtendedData
emptyExtendedData = mkConstr 1 [mkMap []]

{-# INLINABLE setOracleProvider #-}
setOracleProvider :: ExtendedData -> Integer -> ExtendedData
setOracleProvider ex idx = insertConstrSMap 0 (mkI idx) ex

{-# INLINABLE getOracleProvider #-}
getOracleProvider :: ExtendedData -> Maybe Integer
getOracleProvider ed = failExtendedData <$> lookupConstrSMap (mkI 0) ed

{-# INLINABLE setDataSourceCount #-}
setDataSourceCount :: ExtendedData -> Integer -> ExtendedData
setDataSourceCount ex n = insertConstrSMap 1 (mkI n) ex

{-# INLINABLE getDataSourceCount #-}
getDataSourceCount :: ExtendedData -> Maybe Integer
getDataSourceCount ed = failExtendedData <$> lookupConstrSMap (mkI 1) ed

{-# INLINABLE setDataSignatoriesCount #-}
setDataSignatoriesCount :: ExtendedData -> Integer -> ExtendedData
setDataSignatoriesCount ex n = insertConstrSMap 2 (mkI n) ex

{-# INLINABLE getDataSignatoriesCount #-}
getDataSignatoriesCount :: ExtendedData -> Maybe Integer
getDataSignatoriesCount ed = failExtendedData <$> lookupConstrSMap (mkI 2) ed

{-# INLINABLE setOracleProviderSignature #-}
setOracleProviderSignature :: ExtendedData -> BuiltinByteString -> ExtendedData
setOracleProviderSignature ex s = insertConstrSMap 3 (toBuiltinData s) ex

{-# INLINABLE getOracleProviderSignature #-}
getOracleProviderSignature :: ExtendedData -> Maybe BuiltinByteString
getOracleProviderSignature ed = failExtendedData <$> lookupConstrSMap (mkI 3) ed

{-# INLINABLE setExtendedCustomField #-}
setExtendedCustomField :: ToData a => ExtendedData -> Integer -> a -> ExtendedData
setExtendedCustomField pm idx v
  | idx < 0 || idx > 100 = insertConstrSMap idx (toBuiltinData v) pm
  | otherwise            = traceError "setExtendedCustomField: Standard Field Id"

{-# INLINABLE getCustomField #-}
getCustomField :: FromData a => ExtendedData -> Integer -> Maybe a
getCustomField pm idx = failExtendedData <$> lookupConstrSMap (mkI idx) pm

{-# INLINABLE failExtendedData #-}
failExtendedData :: FromData a => BuiltinData -> a
failExtendedData = failIfNotStandard err
  where
    err :: BuiltinString
    err = "ExtendedData: Index not valid standard."