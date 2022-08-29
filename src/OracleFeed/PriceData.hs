module OracleFeed.PriceData where

import PlutusTx.Prelude ( otherwise
                        , Integer
                        , Ord((>), (<))
                        , (||)
                        , traceError
                        )
import Plutus.V1.Ledger.Api ( ToData(..)
                            , POSIXTime
                            , CurrencySymbol
                            , TokenName
                            )
import PlutusTx.Builtins ( mkMap, mkConstr, mkI )

import Utils ( insertMap )
import OracleFeed.Types ( PriceMap, GenericData )

mkPriceData :: [PriceMap] -> GenericData
mkPriceData = mkConstr 2

mkPriceMap :: Integer -> PriceMap
mkPriceMap p = mkMap [(mkI 0, mkI p)]

setTimestamp :: PriceMap -> POSIXTime -> PriceMap
setTimestamp pm t = insertMap 1 (toBuiltinData t) pm

setExpiry :: PriceMap -> POSIXTime -> PriceMap
setExpiry pm v = insertMap 2 (toBuiltinData v) pm

setPrecision :: PriceMap -> Integer -> PriceMap
setPrecision pm v = insertMap 3 (toBuiltinData v) pm

setBaseId :: PriceMap -> Integer -> PriceMap
setBaseId pm v = insertMap 4 (toBuiltinData v) pm

setQuoteId :: PriceMap -> Integer -> PriceMap
setQuoteId pm v = insertMap 5 (toBuiltinData v) pm

setBaseSymbol :: PriceMap -> CurrencySymbol -> PriceMap
setBaseSymbol pm v = insertMap 6 (toBuiltinData v) pm

setQuoteSymbol :: PriceMap -> CurrencySymbol -> PriceMap
setQuoteSymbol pm v = insertMap 7 (toBuiltinData v) pm

setBaseName :: PriceMap -> TokenName -> PriceMap
setBaseName pm v = insertMap 8 (toBuiltinData v) pm

setQuoteName :: PriceMap -> TokenName -> PriceMap
setQuoteName pm v = insertMap 9 (toBuiltinData v) pm

setCustomField :: ToData a => PriceMap -> Integer -> a -> PriceMap
setCustomField pm idx v | idx < 0 || idx > 9 = insertMap idx (toBuiltinData v) pm
                        | otherwise          = traceError "Standard Field Id"
