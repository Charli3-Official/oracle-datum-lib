module Utils where

import PlutusTx (ToData(..), FromData (fromBuiltinData))
import PlutusTx.Prelude
import PlutusTx.Builtins (matchData, mkMap, mkConstr, mkI)

{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = traceError "Nothing"

{-# INLINABLE insertMap #-}
insertMap :: Integer -> BuiltinData -> BuiltinData -> BuiltinData
insertMap idx v pm = matchData pm (const err) (mkMap . insert) err err err
  where
    insert :: [(BuiltinData, BuiltinData)] -> [(BuiltinData, BuiltinData)]
    insert [] = [(toBuiltinData idx, v)]
    insert (p@(k,_):xs) | k==mkI idx = (k,v):xs
                        | otherwise = p:insert xs

    err :: a -> BuiltinData
    err = const $ traceError "insertMap: Not a BuiltinMap"

{-# INLINABLE lookupMap #-}
lookupMap :: BuiltinData -> BuiltinData -> Maybe BuiltinData
lookupMap k pm = matchData pm (const err) lookup err err err
  where
    lookup :: [(BuiltinData, BuiltinData)] -> Maybe BuiltinData
    lookup [] = Nothing
    lookup ((ik, iv):xs) = if ik==k then Just iv else lookup xs

    err :: a -> b
    err = const $ traceError "lookupMap: Not a BuiltinMap"

{-# INLINABLE takeConstr #-}
takeConstr :: BuiltinData -> (Integer, [BuiltinData])
takeConstr c = matchData c (,) err err err err
  where
    err :: a -> b
    err = const $ traceError "takeConstr: Not a Constructor"

{-# INLINABLE insertConstrSMap #-}
insertConstrSMap :: Integer -> BuiltinData -> BuiltinData -> BuiltinData
insertConstrSMap idx v bd = mkConstr n [insertMap idx v bdmap]
  where
    (n,[bdmap]) = takeConstr bd

{-# INLINABLE lookupConstrSMap #-}
lookupConstrSMap :: BuiltinData -> BuiltinData -> Maybe BuiltinData
lookupConstrSMap k pm = lookupMap k imap
  where
    (_,[imap]) = takeConstr pm

{-# INLINABLE failIfNotStandard #-}
failIfNotStandard :: FromData a => BuiltinString -> BuiltinData -> a
failIfNotStandard e v = case fromBuiltinData v of
                            Nothing -> traceError e
                            Just a -> a