module Utils where

import PlutusTx (ToData(..))
import PlutusTx.Prelude
import PlutusTx.Maybe ( Maybe(Just) )
import PlutusTx.Builtins (matchData, mkMap)


fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = traceError "Nothing"

insertMap :: Integer -> BuiltinData -> BuiltinData -> BuiltinData
insertMap idx v pm = matchData pm (const err) insert err err err
  where
    insert :: [(BuiltinData, BuiltinData)] -> BuiltinData
    insert = mkMap . (:) (toBuiltinData idx, v)

    err :: a -> BuiltinData
    err = traceError "Not a BuiltinMap"