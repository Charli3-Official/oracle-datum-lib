{-# LANGUAGE NoImplicitPrelude  #-}

module OracleFeed.Types where

import PlutusTx.Builtins (BuiltinData)

type OracleFeed = BuiltinData

type GenericData = BuiltinData
type SharedData = BuiltinData
type ExtendedData = BuiltinData

type PriceMap = BuiltinData
type PriceData = [PriceMap]
