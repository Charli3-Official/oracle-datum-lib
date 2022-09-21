{-# LANGUAGE RankNTypes #-}
module OracleFeed.PriceData where

import PlutusTx.Prelude
import Plutus.V1.Ledger.Api ( ToData(..)
                            , POSIXTime
                            , CurrencySymbol
                            , TokenName
                            , FromData
                            )
import PlutusTx.Builtins ( mkMap, mkConstr, mkI, matchData )

import Utils ( insertMap, lookupMap, fromJust, failIfNotStandard )
import OracleFeed.Types ( GenericData, PriceData, PriceMap )

{-# INLINABLE mkPriceData #-}
mkPriceData :: PriceMap -> GenericData
mkPriceData pm = mkConstr 2 arr
  where
    arr :: [PriceMap]
    arr = case lookupMap (mkI 0) pm of
      Nothing -> traceError "PriceMap does not have a price"
      Just _ -> [pm]

{-# INLINABLE mkPriceMap #-}
mkPriceMap :: Integer -> PriceMap
mkPriceMap p = mkMap [(mkI 0, mkI p)]

{-# INLINABLE getPrice #-}
getPrice :: PriceMap -> Integer
getPrice pm = fromJust $ failPriceData <$> lookupMap (mkI 0) pm

{-# INLINABLE setTimestamp #-}
setTimestamp :: PriceMap -> POSIXTime -> PriceMap
setTimestamp pm t = insertMap 1 (toBuiltinData t) pm

{-# INLINABLE getTimestamp #-}
getTimestamp :: PriceMap -> Maybe POSIXTime
getTimestamp pm = failPriceData <$> lookupMap (mkI 1) pm

{-# INLINABLE setExpiry #-}
setExpiry :: PriceMap -> POSIXTime -> PriceMap
setExpiry pm v = insertMap 2 (toBuiltinData v) pm

{-# INLINABLE getExpiry #-}
getExpiry :: PriceMap -> Maybe POSIXTime
getExpiry pm = failPriceData <$> lookupMap (mkI 2) pm

{-# INLINABLE setPrecision #-}
setPrecision :: PriceMap -> Integer -> PriceMap
setPrecision pm v = insertMap 3 (toBuiltinData v) pm

{-# INLINABLE getPrecision #-}
getPrecision :: PriceMap -> Maybe Integer
getPrecision pm = failPriceData <$> lookupMap (mkI 3) pm

{-# INLINABLE setBaseId #-}
setBaseId :: PriceMap -> Integer -> PriceMap
setBaseId pm v = insertMap 4 (toBuiltinData v) pm

{-# INLINABLE getBaseId #-}
getBaseId :: PriceMap -> Maybe Integer
getBaseId pm = failPriceData <$> lookupMap (mkI 4) pm

{-# INLINABLE setQuoteId #-}
setQuoteId :: PriceMap -> Integer -> PriceMap
setQuoteId pm v = insertMap 5 (toBuiltinData v) pm

{-# INLINABLE getQuoteId #-}
getQuoteId :: PriceMap -> Maybe Integer
getQuoteId pm = failPriceData <$> lookupMap (mkI 5) pm

{-# INLINABLE setBaseSymbol #-}
setBaseSymbol :: PriceMap -> CurrencySymbol -> PriceMap
setBaseSymbol pm v = insertMap 6 (toBuiltinData v) pm

{-# INLINABLE getBaseSymbol #-}
getBaseSymbol :: PriceMap -> Maybe CurrencySymbol
getBaseSymbol pm = failPriceData <$> lookupMap (mkI 6) pm

{-# INLINABLE setQuoteSymbol #-}
setQuoteSymbol :: PriceMap -> CurrencySymbol -> PriceMap
setQuoteSymbol pm v = insertMap 7 (toBuiltinData v) pm

{-# INLINABLE getQuoteSymbol #-}
getQuoteSymbol :: PriceMap -> Maybe CurrencySymbol
getQuoteSymbol pm = failPriceData <$> lookupMap (mkI 7) pm

{-# INLINABLE setBaseName #-}
setBaseName :: PriceMap -> TokenName -> PriceMap
setBaseName pm v = insertMap 8 (toBuiltinData v) pm

{-# INLINABLE getBaseName #-}
getBaseName :: PriceMap -> Maybe TokenName
getBaseName pm = failPriceData <$> lookupMap (mkI 8) pm

{-# INLINABLE setQuoteName #-}
setQuoteName :: PriceMap -> TokenName -> PriceMap
setQuoteName pm v = insertMap 9 (toBuiltinData v) pm

{-# INLINABLE getQuoteName #-}
getQuoteName :: PriceMap -> Maybe TokenName
getQuoteName pm = failPriceData <$> lookupMap (mkI 9) pm

{-# INLINABLE setPriceCustomField #-}
setPriceCustomField :: ToData a => PriceMap -> Integer -> a -> PriceMap
setPriceCustomField pm idx v | idx < 0 || idx > 9 = insertMap idx (toBuiltinData v) pm
                             | otherwise          = traceError "Standard Field Id"

{-# INLINABLE getPriceCustomField #-}
getPriceCustomField :: FromData a => PriceMap -> Integer -> Maybe a
getPriceCustomField pm idx = failPriceData <$> lookupMap (mkI idx) pm

{-# INLINABLE getPriceMap #-}
getPriceMap :: PriceData -> PriceMap
getPriceMap pd = matchData pd (\_ [xs] -> xs) err err err err
  where
    err :: a -> b
    err = const $ traceError "Invalid PriceData: Not a constructor"

{-# INLINABLE failPriceData #-}
failPriceData :: FromData a => BuiltinData -> a
failPriceData = failIfNotStandard err
  where
    err :: BuiltinString
    err = "PriceData: Index not valid standard."