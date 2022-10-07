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
emptySharedData = SharedData $ mkConstr 0 [mkMap []]

{-# INLINABLE emptySharedPriceMap #-}
emptySharedPriceMap :: PriceMap
emptySharedPriceMap = PriceMap $ mkMap []

{-# INLINABLE setSharedPriceData #-}
setSharedPriceData :: PriceMap -> SharedData -> SharedData
setSharedPriceData pm = mapShD (insertConstrSMap 0 $ takePM pm)

{-# INLINABLE getSharedPriceData #-}
getSharedPriceData :: SharedData -> Maybe PriceMap
getSharedPriceData sd = PriceMap <$> lookupConstrSMap (mkI 0) (takeShD sd)

mapShD :: (BuiltinData -> BuiltinData) -> SharedData -> SharedData
mapShD f = SharedData . f . takeShD