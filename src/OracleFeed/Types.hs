{-# LANGUAGE InstanceSigs #-}
module OracleFeed.Types where

import qualified Prelude as HP

import PlutusTx.Prelude (Maybe(..), (.), Eq(..), Bool)
import PlutusTx.Builtins (BuiltinData)
import Plutus.V2.Ledger.Api (ToData(..), FromData(..), UnsafeFromData(..))

newtype OracleFeed = OracleFeed BuiltinData
    deriving (HP.Eq, HP.Show)

newtype GenericData = GenericData BuiltinData
newtype ExtendedData = ExtendedData BuiltinData
newtype SharedData = SharedData BuiltinData

newtype PriceMap = PriceMap BuiltinData
newtype PriceData = PriceData BuiltinData

{-# INLINABLE takeOF #-}
takeOF :: OracleFeed -> BuiltinData
takeOF (OracleFeed a) = a

{-# INLINABLE takeGD #-}
takeGD :: GenericData -> BuiltinData
takeGD (GenericData a) = a

{-# INLINABLE takeExD #-}
takeExD :: ExtendedData -> BuiltinData
takeExD (ExtendedData a) = a

{-# INLINABLE takeShD #-}
takeShD :: SharedData -> BuiltinData
takeShD (SharedData a) = a

{-# INLINABLE takePM #-}
takePM :: PriceMap -> BuiltinData
takePM (PriceMap a) = a

{-# INLINABLE takePD #-}
takePD :: PriceData -> BuiltinData
takePD (PriceData a) = a

instance Eq OracleFeed where
    (==) :: OracleFeed -> OracleFeed -> Bool
    (OracleFeed of1) == (OracleFeed of2) = of1 == of2

instance ToData OracleFeed where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData :: OracleFeed -> BuiltinData
    toBuiltinData = takeOF

instance FromData OracleFeed where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData :: BuiltinData -> Maybe OracleFeed
    fromBuiltinData = Just . OracleFeed

instance UnsafeFromData OracleFeed where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData :: BuiltinData -> OracleFeed
    unsafeFromBuiltinData = OracleFeed
