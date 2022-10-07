{-# LANGUAGE RankNTypes #-}
module OracleFeed.PriceData
  ( mkPriceData
  , mkPriceMap
  , setPrice
  , getPrice
  , setTimestamp
  , getTimestamp
  , setExpiry
  , getExpiry
  , setPrecision
  , getPrecision
  , setBaseId
  , getBaseId
  , setQuoteId
  , getQuoteId
  , setBaseSymbol
  , getBaseSymbol
  , setQuoteSymbol
  , getQuoteSymbol
  , setBaseName
  , getBaseName
  , setQuoteName
  , getQuoteName
  , setPriceCustomField
  , getPriceCustomField
  , getPriceMap
  ) where

import PlutusTx.Prelude
import Plutus.V1.Ledger.Api ( ToData(..)
                            , POSIXTime
                            , CurrencySymbol
                            , TokenName
                            , FromData
                            )
import PlutusTx.Builtins ( mkMap, mkConstr, mkI, matchData )

import Utils ( insertMap, lookupMap, fromJust, failIfNotStandard )
import OracleFeed.Types ( GenericData (GenericData), PriceData, PriceMap (PriceMap), takePM, takePD )

{-# INLINABLE mkPriceData #-}
mkPriceData :: PriceMap -> GenericData
mkPriceData (PriceMap pm) = GenericData $ mkConstr 2 arr
  where
    arr :: [BuiltinData]
    arr = case lookupMap (mkI 0) pm of
      Nothing -> traceError "PriceMap does not have a price"
      Just _ -> [pm]

{-# INLINABLE mkPriceMap #-}
mkPriceMap :: Integer -> PriceMap
mkPriceMap p = PriceMap $ mkMap [(mkI 0, mkI p)]

{-# INLINABLE setPrice #-}
setPrice :: Integer -> PriceMap -> PriceMap
setPrice v = mapPM $ insertMap 0 (toBuiltinData v)

{-# INLINABLE getPrice #-}
getPrice :: PriceMap -> Integer
getPrice pm = fromJust $ failPriceData <$> lookupMap (mkI 0) (takePM pm)

{-# INLINABLE setTimestamp #-}
setTimestamp :: POSIXTime -> PriceMap -> PriceMap
setTimestamp t = mapPM $ insertMap 1 (toBuiltinData t)

{-# INLINABLE getTimestamp #-}
getTimestamp :: PriceMap -> Maybe POSIXTime
getTimestamp pm = failPriceData <$> lookupMap (mkI 1) (takePM pm)

{-# INLINABLE setExpiry #-}
setExpiry :: POSIXTime -> PriceMap -> PriceMap
setExpiry v = mapPM $ insertMap 2 (toBuiltinData v)

{-# INLINABLE getExpiry #-}
getExpiry :: PriceMap -> Maybe POSIXTime
getExpiry pm = failPriceData <$> lookupMap (mkI 2) (takePM pm)

{-# INLINABLE setPrecision #-}
setPrecision :: Integer -> PriceMap -> PriceMap
setPrecision v = mapPM $ insertMap 3 (toBuiltinData v)

{-# INLINABLE getPrecision #-}
getPrecision :: PriceMap -> Maybe Integer
getPrecision pm = failPriceData <$> lookupMap (mkI 3) (takePM pm)

{-# INLINABLE setBaseId #-}
setBaseId :: Integer -> PriceMap -> PriceMap
setBaseId v = mapPM $ insertMap 4 (toBuiltinData v)

{-# INLINABLE getBaseId #-}
getBaseId :: PriceMap -> Maybe Integer
getBaseId pm = failPriceData <$> lookupMap (mkI 4) (takePM pm)

{-# INLINABLE setQuoteId #-}
setQuoteId :: Integer -> PriceMap -> PriceMap
setQuoteId v = mapPM $ insertMap 5 (toBuiltinData v)

{-# INLINABLE getQuoteId #-}
getQuoteId :: PriceMap -> Maybe Integer
getQuoteId pm = failPriceData <$> lookupMap (mkI 5) (takePM pm)

{-# INLINABLE setBaseSymbol #-}
setBaseSymbol :: CurrencySymbol -> PriceMap -> PriceMap
setBaseSymbol v = mapPM $ insertMap 6 (toBuiltinData v)

{-# INLINABLE getBaseSymbol #-}
getBaseSymbol :: PriceMap -> Maybe CurrencySymbol
getBaseSymbol pm = failPriceData <$> lookupMap (mkI 6) (takePM pm)

{-# INLINABLE setQuoteSymbol #-}
setQuoteSymbol :: CurrencySymbol -> PriceMap -> PriceMap
setQuoteSymbol v = mapPM $ insertMap 7 (toBuiltinData v)

{-# INLINABLE getQuoteSymbol #-}
getQuoteSymbol :: PriceMap -> Maybe CurrencySymbol
getQuoteSymbol pm = failPriceData <$> lookupMap (mkI 7) (takePM pm)

{-# INLINABLE setBaseName #-}
setBaseName :: TokenName -> PriceMap -> PriceMap
setBaseName v = mapPM $ insertMap 8 (toBuiltinData v)

{-# INLINABLE getBaseName #-}
getBaseName :: PriceMap -> Maybe TokenName
getBaseName pm = failPriceData <$> lookupMap (mkI 8) (takePM pm)

{-# INLINABLE setQuoteName #-}
setQuoteName :: TokenName -> PriceMap -> PriceMap
setQuoteName v = mapPM $ insertMap 9 (toBuiltinData v)

{-# INLINABLE getQuoteName #-}
getQuoteName :: PriceMap -> Maybe TokenName
getQuoteName pm = failPriceData <$> lookupMap (mkI 9) (takePM pm)

{-# INLINABLE setPriceCustomField #-}
setPriceCustomField :: ToData a => Integer -> a -> PriceMap -> PriceMap
setPriceCustomField idx v | idx < 0 || idx > 9 = mapPM $ insertMap idx (toBuiltinData v)
                          | otherwise          = traceError "Standard Field Id"

{-# INLINABLE getPriceCustomField #-}
getPriceCustomField :: FromData a => PriceMap -> Integer -> Maybe a
getPriceCustomField pm idx = failPriceData <$> lookupMap (mkI idx) (takePM pm)

{-# INLINABLE getPriceMap #-}
getPriceMap :: PriceData -> PriceMap
getPriceMap pd = PriceMap $ matchData (takePD pd) (\_ [xs] -> xs) err err err err
  where
    err :: a -> b
    err = const $ traceError "Invalid PriceData: Not a constructor"

{-# INLINABLE failPriceData #-}
failPriceData :: FromData a => BuiltinData -> a
failPriceData = failIfNotStandard err
  where
    err :: BuiltinString
    err = "PriceData: Index not valid standard."

{-# INLINABLE mapPM #-}
mapPM :: (BuiltinData -> BuiltinData) -> PriceMap -> PriceMap
mapPM f = PriceMap . f . takePM