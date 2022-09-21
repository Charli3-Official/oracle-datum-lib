module OracleFeed.SharedData where

import OracleFeed.Types
import PlutusTx.Builtins (mkMap, mkConstr, mkI)
import Utils ( insertConstrSMap, lookupConstrSMap )
import PlutusTx.Prelude

{-# INLINABLE emptySharedData #-}
emptySharedData :: SharedData
emptySharedData = mkConstr 0 [mkMap []]

{-# INLINABLE emptySharedPriceMap #-}
emptySharedPriceMap :: PriceMap
emptySharedPriceMap = mkMap []

{-# INLINABLE setSharedPriceData #-}
setSharedPriceData :: SharedData -> PriceMap -> SharedData
setSharedPriceData sd pm = insertConstrSMap 0 pm sd

{-# INLINABLE getSharedPriceData #-}
getSharedPriceData :: SharedData -> Maybe PriceMap
getSharedPriceData = lookupConstrSMap (mkI 0)
