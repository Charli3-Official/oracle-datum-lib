module OracleFeed.SharedData
  ( emptySharedData
  , emptySharedPriceMap
  , setSharedPriceData
  , getSharedPriceData
  ) where

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
setSharedPriceData :: PriceMap -> SharedData -> SharedData
setSharedPriceData pm = insertConstrSMap 0 pm

{-# INLINABLE getSharedPriceData #-}
getSharedPriceData :: SharedData -> Maybe PriceMap
getSharedPriceData = lookupConstrSMap (mkI 0)
