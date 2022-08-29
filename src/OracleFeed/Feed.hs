module OracleFeed.Feed where

import PlutusTx.Prelude ( Maybe
                        , BuiltinData
                        , ($)
                        , (++)
                        , isJust
                        )
import PlutusTx.Builtins ( mkConstr )

import OracleFeed.Types ( ExtendedData
                        , SharedData
                        , GenericData
                        , OracleFeed
                        )
import Utils ( fromJust )

mkOracleFeed :: Maybe SharedData -> [GenericData] -> Maybe ExtendedData -> OracleFeed
mkOracleFeed sd gds ed = mkConstr 0 $ sharedS++gds++extendedS
  where
    sharedS :: [BuiltinData]
    sharedS = [fromJust sd | isJust sd]

    extendedS :: [BuiltinData]
    extendedS = [fromJust ed | isJust ed]
