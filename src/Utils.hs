module Utils where

import PlutusTx (ToData(..), FromData (fromBuiltinData))
import PlutusTx.Prelude
import PlutusTx.Builtins (matchData, mkMap)

{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = traceError "Nothing"

{-# INLINABLE insertMap #-}
insertMap :: Integer -> BuiltinData -> BuiltinData -> BuiltinData
insertMap idx v pm = matchData pm (const err) insert err err err
  where
    insert :: [(BuiltinData, BuiltinData)] -> BuiltinData
    insert = mkMap . (:) (toBuiltinData idx, v)

    err :: a -> BuiltinData
    err = const $ traceError "Not a BuiltinMap"

{-# INLINABLE lookupMap #-}
lookupMap :: BuiltinData -> BuiltinData -> Maybe BuiltinData
lookupMap k pm = matchData pm (const err) lookup err err err
  where
    lookup :: [(BuiltinData, BuiltinData)] -> Maybe BuiltinData
    lookup [] = Nothing
    lookup ((ik, iv):xs) = if ik==k then Just iv else lookup xs

    err :: a -> b
    err = const $ traceError "Invalid PriceMap: Not a BuiltinMap"

{-# INLINABLE failIfNotStandard #-}
failIfNotStandard :: FromData a => BuiltinString -> BuiltinData -> a
failIfNotStandard e v = case fromBuiltinData v of
                            Nothing -> traceError e
                            Just a -> a