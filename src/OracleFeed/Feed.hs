module OracleFeed.Feed
  ( mkOracleFeed
  , getPriceDatas
  , getSharedData
  , getExtendedData
  )
where

import PlutusTx.Prelude
import PlutusTx.Builtins ( mkConstr, matchData )

import OracleFeed.Types
import Utils ( fromJust )

{-# INLINABLE mkOracleFeed #-}
mkOracleFeed :: Maybe SharedData -> [GenericData] -> Maybe ExtendedData -> OracleFeed
mkOracleFeed sd gds ed = OracleFeed $ mkConstr 0 $ sharedS++(takeGD <$> gds)++extendedS
  where
    sharedS :: [BuiltinData]
    sharedS = [takeShD $ fromJust sd | isJust sd]

    extendedS :: [BuiltinData]
    extendedS = [takeExD $ fromJust ed | isJust ed]

{-# INLINABLE getPriceDatas #-}
getPriceDatas :: OracleFeed -> [PriceData]
getPriceDatas ofeed = PriceData <$> filterOFeed ofeed 2

{-# INLINABLE getSharedData #-}
getSharedData :: OracleFeed -> Maybe SharedData
getSharedData ofeed = listToMaybe $ SharedData <$> filterOFeed ofeed 0

{-# INLINABLE getExtendedData #-}
getExtendedData :: OracleFeed -> Maybe ExtendedData
getExtendedData ofeed = listToMaybe $ ExtendedData <$> filterOFeed ofeed 1

{-# INLINABLE filterOFeed #-}
filterOFeed :: OracleFeed -> Integer -> [BuiltinData]
filterOFeed ofeed c = matchData (takeOF ofeed) filterData err err err err
  where
    filterData :: Integer -> [BuiltinData] -> [BuiltinData]
    filterData a xs | a==0 = filterData' xs
                    | otherwise = traceError "Invalid OracleFeed: Not an OracleFeed"

    filterData' :: [BuiltinData] -> [BuiltinData]
    filterData' [] = []
    filterData' (x:xs) = isConstrData x++filterData' xs

    isConstrData :: BuiltinData -> [BuiltinData]
    isConstrData d = matchData d (\n xs -> [mkConstr c xs | n==c] ) err err err err

    err :: a -> [BuiltinData]
    err = const $ traceError "Invalid OracleFeed: Not a constructor"
