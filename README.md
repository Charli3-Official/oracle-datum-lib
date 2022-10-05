# oracle-feed
Library to interact with and create a standard oracle datum.

# Introduction
This library's objective is to provide an abstraction layer over the BuiltinData of a Standard Oracle Datum. It is implemented with the `PlutusTx` library as well as using the `INLINABLE` pragma to allow for its use in OnChain Plutus code. It implements getters and setters for each generic type in the CIP and for Standard and Extended Data. The cddl can be found on the `spec.cddl` file in the root of this repository.

# Error reporting
This library is mostly focused for OnChain code, thus we test the compiled plutus code and use the `traceError` function to report errors, this means that on OffChain Code the error code will not be descriptive. We've decided against using the type `Either` to prevent the interface getting bloated. If the library is being correctly used there should not be any errors.

# Example Usage
* Building an OracleFeed with just a price
```haskell
import OracleFeed

myFeed :: OracleFeed
myFeed = mkOracleFeed Nothing [pd] Nothing
  where
    pd :: GenericData
    pd = mkPriceData $ mkPriceMap 5
```

* Quering an OracleFeed for a price map
```haskell
import OracleFeed

myFeed :: OracleFeed
myFeed = mkOracleFeed Nothing [pd] Nothing
  where
    pd :: GenericData
    pd = mkPriceData
        $ setPrecision 6
        $ setBaseName "USD"
        $ setQuoteName "ADA"
        $ mkPriceMap 500000

queryPriceMap :: OracleFeed -> PriceMap
queryPriceMap = getPriceMap . head . getPriceDatas

myFeedsPriceMap :: PriceMap
myFeedsPriceMap = queryPriceMap myFeed
```

# Modules
## Feed

```haskell
mkOracleFeed :: Maybe SharedData -> [GenericData] -> Maybe ExtendedData -> OracleFeed
```
Builds the oracle feed datum.

```haskell
getPriceDatas :: OracleFeed -> [PriceData]
```
Filters the Feed for all Price Data structures

```haskell
getSharedData :: OracleFeed -> Maybe SharedData
```
Returns the SharedData if it present.

```haskell
getExtendedData :: OracleFeed -> Maybe ExtendedData
```
Returns the ExtendedData if it is present.

## PriceData

```haskell
mkPriceData :: PriceMap -> GenericData
```
Takes a PriceMap and turns it into a GenericData to use in the Oracle Feed.

```haskell
mkPriceMap :: Integer -> PriceMap
```
Creates an PriceMap with the given price.

```haskell
setPrice :: Integer -> PriceMap -> PriceMap
```
Updates the price on a PriceMap.

```haskell
getPrice :: PriceMap -> Integer
```
Returns the price on a PriceMap. This function will fail if the PriceMap does not have a Price (i.e. a shared price map)

```haskell
setTimestamp :: POSIXTime -> PriceMap -> PriceMap
```
Updates the Timestamp on the provided PriceMap.

```haskell
getTimestamp :: PriceMap -> Maybe POSIXTime
```
Returns the Timestamp if it is present on the provided PriceMap.

```haskell
setExpiry :: POSIXTime -> PriceMap -> PriceMap
```
Updates the Expiry on the provided PriceMap.

```haskell
getExpiry :: PriceMap -> Maybe POSIXTime
```
Returns the Expiry if it is present on the provided PriceMap.

```haskell
setPrecision :: Integer -> PriceMap -> PriceMap
```
Updates the Precision on the provided PriceMap.

```haskell
getPrecision :: PriceMap -> Maybe Integer
```
Returns the Precision if it is present on the provided PriceMap.

```haskell
setBaseId :: Integer -> PriceMap -> PriceMap
```
Updates the Base currency ID on the provided PriceMap.

```haskell
getBaseId :: PriceMap -> Maybe Integer
```
Returns the Base currency ID if it is present on the provided PriceMap.

```haskell
setQuoteId :: Integer -> PriceMap -> PriceMap
```
Updates the Quote currency ID on the provided PriceMap.

```haskell
getQuoteId :: PriceMap -> Maybe Integer
```
Returns the Quote currency ID if it is present on the provided PriceMap.

```haskell
setBaseSymbol :: CurrencySymbol -> PriceMap -> PriceMap
```
Updates the Base currency symbol on the provided PriceMap.

```haskell
getBaseSymbol :: PriceMap -> Maybe CurrencySymbol
```
Returns the Base currency symbol if it is present on the provided PriceMap.

```haskell
setQuoteSymbol :: CurrencySymbol -> PriceMap -> PriceMap
```
Updates the Quote surrency symbol on the provided PriceMap.

```haskell
getQuoteSymbol :: PriceMap -> Maybe CurrencySymbol
```
Returns the Quote currency symbol if it is present on the provided PriceMap.

```haskell
setBaseName :: TokenName -> PriceMap -> PriceMap
```
Updates the Base currency name on the provided PriceMap.

```haskell
getBaseName :: PriceMap -> Maybe TokenName
```
Returns the Base currency name if it is present on the provided PriceMap.

```haskell
setQuoteName :: TokenName -> PriceMap -> PriceMap
```
Updates the Quote currency name on the provided PriceMap.

```haskell
getQuoteName :: PriceMap -> Maybe TokenName
```
Returns the Quote currency name if it is present on the provided PriceMap.

```haskell
setPriceCustomField :: ToData a => Integer -> a -> PriceMap -> PriceMap
```
Updates a Custom Field with the provided Integer on the provided PriceMap.

```haskell
getPriceCustomField :: FromData a => PriceMap -> Integer -> Maybe a
```
Returns a Custom Field with the given index if it is present on the provided PriceMap.

```haskell
getPriceMap :: PriceData -> PriceMap
```
Unwraps the PriceMap from the PriceData.

## SharedData

```haskell
emptySharedData :: SharedData
```
Base Structure for the SharedData Constructor

```haskell
emptySharedPriceMap :: PriceMap
```
Base Structure for the shared PriceMap. To set fields in this structure use the setters on the Price Data module.

```haskell
setSharedPriceData :: PriceMap -> SharedData -> SharedData
```
Updates the PriceMap inside a SharedData

```haskell
getSharedPriceData :: SharedData -> Maybe PriceMap
```
Returns the Shared PriceMap if it is present.

## ExtendedData
```haskell
emptyExtendedData :: ExtendedData
```
Base Structure for the ExtendedData Constructor.

```haskell
setOracleProvider :: Integer -> ExtendedData -> ExtendedData
```
Updates the Oracle Provider Id on the ExtendedData.

```haskell
getOracleProvider :: ExtendedData -> Maybe Integer
```
Returns on the Oracle Provider Id if it is present.

```haskell
setDataSourceCount :: Integer -> ExtendedData -> ExtendedData
```
Updates the Data Source Count in the ExtendedData.

```haskell
getDataSourceCount :: ExtendedData -> Maybe Integer
```
Returns the Data Source Count in the ExtendedData.

```haskell
setDataSignatoriesCount :: Integer -> ExtendedData -> ExtendedData
```
Updates the Data Signatories Count in the ExtendedData.

```haskell
getDataSignatoriesCount :: ExtendedData -> Maybe Integer
```
Returns the Data Signatories Count if it is present.

```haskell
setOracleProviderSignature :: BuiltinByteString -> ExtendedData -> ExtendedData
```
Updates the Oracle Provider Signature in the ExtendedData.

```haskell
getOracleProviderSignature :: ExtendedData -> Maybe BuiltinByteString
```
Returns the Oracle Provider Signature if it is present.

```haskell
setExtendedCustomField :: ToData a => Integer -> a -> ExtendedData -> ExtendedData
```
Sets a custom field in the ExtendedData. As the standard describes only indexes from 100 up are available for public use. This function will fail if a non standard index is passed

```haskell
getExtendedCustomField :: FromData a => ExtendedData -> Integer -> Maybe a
```
Returns a field with the given index if it is present.
