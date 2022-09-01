{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Compiled.PriceData where

import PlutusTx.TH (compile)
import PlutusTx (CompiledCode)

import OracleFeed
import Plutus.V1.Ledger.Api (POSIXTime, TokenName)
import PlutusTx.Prelude ( Integer, Maybe, BuiltinData )
import Plutus.V1.Ledger.Value (CurrencySymbol)

mkPriceDataC :: CompiledCode ([BuiltinData] -> BuiltinData)
mkPriceDataC = $$(compile [|| mkPriceData ||])

mkPriceMapC :: CompiledCode (Integer -> PriceMap)
mkPriceMapC = $$(compile [|| mkPriceMap ||])

getPriceC :: CompiledCode (PriceMap -> Integer)
getPriceC = $$(compile [|| getPrice ||])

setTimestampC :: CompiledCode (PriceMap -> POSIXTime -> PriceMap)
setTimestampC = $$(compile [|| setTimestamp ||])

getTimestampC :: CompiledCode (PriceMap -> Maybe POSIXTime)
getTimestampC = $$(compile [|| getTimestamp ||])

setExpiryC :: CompiledCode (PriceMap -> POSIXTime -> PriceMap)
setExpiryC = $$(compile [|| setExpiry ||])

getExpiryC :: CompiledCode (PriceMap -> Maybe POSIXTime)
getExpiryC = $$(compile [|| getExpiry ||])

setPrecisionC :: CompiledCode (PriceMap -> Integer -> PriceMap)
setPrecisionC = $$(compile [|| setPrecision ||])

getPrecisionC :: CompiledCode (PriceMap -> Maybe Integer)
getPrecisionC = $$(compile [|| getPrecision ||])

setBaseIdC :: CompiledCode (PriceMap -> Integer -> PriceMap)
setBaseIdC = $$(compile [|| setBaseId ||])

getBaseIdC :: CompiledCode (PriceMap -> Maybe Integer)
getBaseIdC = $$(compile [|| getBaseId ||])

setQuoteIdC :: CompiledCode (PriceMap -> Integer -> PriceMap)
setQuoteIdC = $$(compile [|| setQuoteId ||])

getQuoteIdC :: CompiledCode (PriceMap -> Maybe Integer)
getQuoteIdC = $$(compile [|| getQuoteId ||])

setBaseSymbolC :: CompiledCode (PriceMap -> CurrencySymbol -> PriceMap)
setBaseSymbolC = $$(compile [|| setBaseSymbol ||])

getBaseSymbolC :: CompiledCode (PriceMap -> Maybe CurrencySymbol)
getBaseSymbolC = $$(compile [|| getBaseSymbol ||])

setQuoteSymbolC :: CompiledCode (PriceMap -> CurrencySymbol -> PriceMap)
setQuoteSymbolC = $$(compile [|| setQuoteSymbol ||])

getQuoteSymbolC :: CompiledCode (PriceMap -> Maybe CurrencySymbol)
getQuoteSymbolC = $$(compile [|| getQuoteSymbol ||])

setBaseNameC :: CompiledCode (PriceMap -> TokenName -> PriceMap)
setBaseNameC = $$(compile [|| setBaseName ||])

getBaseNameC :: CompiledCode (PriceMap -> Maybe TokenName)
getBaseNameC = $$(compile [|| getBaseName ||])

setQuoteNameC :: CompiledCode (PriceMap -> TokenName -> PriceMap)
setQuoteNameC = $$(compile [|| setQuoteName ||])

getQuoteNameC :: CompiledCode (PriceMap -> Maybe TokenName)
getQuoteNameC = $$(compile [|| getQuoteName ||])

-- setCustomFieldC :: ToData a => CompiledCode (PriceMap -> Integer -> a -> PriceMap)
-- setCustomFieldC = $$(compile [|| setCustomField ||])

-- getCustomFieldC :: FromData a => CompiledCode (PriceMap -> Integer -> Maybe a)
-- getCustomFieldC = $$(compile [|| getCustomField ||])

getPriceMapsC :: CompiledCode (PriceData -> [PriceMap])
getPriceMapsC = $$(compile [|| getPriceMaps ||])