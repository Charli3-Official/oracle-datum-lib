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

{-# INLINABLE setPrice #-}
setPrice :: Integer -> PriceMap -> PriceMap
setPrice v pm = insertMap 0 (toBuiltinData v) arr
  where
    arr :: PriceMap
    arr = case lookupMap (mkI 0) pm of
            Nothing -> traceError "can't set a price to shared map"
            Just _ -> pm

{-# INLINABLE getPrice #-}
getPrice :: PriceMap -> Integer
getPrice pm = fromJust $ failPriceData <$> lookupMap (mkI 0) pm

{-# INLINABLE setTimestamp #-}
setTimestamp :: POSIXTime -> PriceMap -> PriceMap
setTimestamp t = insertMap 1 (toBuiltinData t)

{-# INLINABLE getTimestamp #-}
getTimestamp :: PriceMap -> Maybe POSIXTime
getTimestamp pm = failPriceData <$> lookupMap (mkI 1) pm

{-# INLINABLE setExpiry #-}
setExpiry :: POSIXTime -> PriceMap -> PriceMap
setExpiry v = insertMap 2 (toBuiltinData v)

{-# INLINABLE getExpiry #-}
getExpiry :: PriceMap -> Maybe POSIXTime
getExpiry pm = failPriceData <$> lookupMap (mkI 2) pm

{-# INLINABLE setPrecision #-}
setPrecision :: Integer -> PriceMap -> PriceMap
setPrecision v = insertMap 3 (toBuiltinData v)

{-# INLINABLE getPrecision #-}
getPrecision :: PriceMap -> Maybe Integer
getPrecision pm = failPriceData <$> lookupMap (mkI 3) pm

{-# INLINABLE setBaseId #-}
setBaseId :: Integer -> PriceMap -> PriceMap
setBaseId v = insertMap 4 (toBuiltinData v)

{-# INLINABLE getBaseId #-}
getBaseId :: PriceMap -> Maybe Integer
getBaseId pm = failPriceData <$> lookupMap (mkI 4) pm

{-# INLINABLE setQuoteId #-}
setQuoteId :: Integer -> PriceMap -> PriceMap
setQuoteId v = insertMap 5 (toBuiltinData v)

{-# INLINABLE getQuoteId #-}
getQuoteId :: PriceMap -> Maybe Integer
getQuoteId pm = failPriceData <$> lookupMap (mkI 5) pm

{-# INLINABLE setBaseSymbol #-}
setBaseSymbol :: CurrencySymbol -> PriceMap -> PriceMap
setBaseSymbol v = insertMap 6 (toBuiltinData v)

{-# INLINABLE getBaseSymbol #-}
getBaseSymbol :: PriceMap -> Maybe CurrencySymbol
getBaseSymbol pm = failPriceData <$> lookupMap (mkI 6) pm

{-# INLINABLE setQuoteSymbol #-}
setQuoteSymbol :: CurrencySymbol -> PriceMap -> PriceMap
setQuoteSymbol v = insertMap 7 (toBuiltinData v)

{-# INLINABLE getQuoteSymbol #-}
getQuoteSymbol :: PriceMap -> Maybe CurrencySymbol
getQuoteSymbol pm = failPriceData <$> lookupMap (mkI 7) pm

{-# INLINABLE setBaseName #-}
setBaseName :: TokenName -> PriceMap -> PriceMap
setBaseName v = insertMap 8 (toBuiltinData v)

{-# INLINABLE getBaseName #-}
getBaseName :: PriceMap -> Maybe TokenName
getBaseName pm = failPriceData <$> lookupMap (mkI 8) pm

{-# INLINABLE setQuoteName #-}
setQuoteName :: TokenName -> PriceMap -> PriceMap
setQuoteName v = insertMap 9 (toBuiltinData v)

{-# INLINABLE getQuoteName #-}
getQuoteName :: PriceMap -> Maybe TokenName
getQuoteName pm = failPriceData <$> lookupMap (mkI 9) pm

{-# INLINABLE setPriceCustomField #-}
setPriceCustomField :: ToData a => Integer -> a -> PriceMap -> PriceMap
setPriceCustomField idx v | idx < 0 || idx > 9 = insertMap idx (toBuiltinData v)
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