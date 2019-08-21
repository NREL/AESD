{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for data values.
-}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module AESD.Types.Value (
-- * Data values
  DataValue
, realValue
, integerValue
, stringValue
, onDataValue
, onDataValue2
, onDataValues
-- * Variable types
, VarType(..)
, varType
, varTypes
, sameVarTypes
, commonVarType
, commonVarTypes
, castVarType
, castVarTypes
) where


import Control.Applicative ((<|>))
import Data.Int (Int64)
import Data.List (union)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.ProtocolBuffers (Decode, Encode, Optional, Value, getField, putField)
import GHC.Generics (Generic)
import Text.Read (readMaybe)


-- | A data value.
data DataValue =
  DataValue
  {
    realValue'    :: Optional 1 (Value Double)
  , integerValue' :: Optional 2 (Value Int64 )
  , stringValue'  :: Optional 3 (Value String)
  }
    deriving (Generic, Show)

instance Eq DataValue where
  (==) = onDataValue2 (==) (==) (==) False

instance Ord DataValue where
  compare x y = onDataValue2 compare compare compare (compare x y) x y

instance Read DataValue where
  readsPrec _ x =
    [
      (
        DataValue
        {
          realValue'    = putField $ readMaybe x
        , integerValue' = putField $ readMaybe x
        , stringValue'  = putField $ Just      x
        }
      , ""
      )
    ]

instance Decode DataValue

instance Encode DataValue


-- | Construct a real value.
realValue :: Double -> DataValue
realValue x = 
  DataValue
  {
    realValue'    = putField $ Just x
  , integerValue' = mempty
  , stringValue'  = mempty
  }


-- | Construct an integer value.
integerValue :: Int64 -> DataValue
integerValue x = 
  DataValue
  {
    realValue'    = mempty
  , integerValue' = putField $ Just x
  , stringValue'  = mempty
  }


-- | Construct a string value.
stringValue :: String -> DataValue
stringValue x = 
  DataValue
  {
    realValue'    = mempty
  , integerValue' = mempty
  , stringValue' = putField $ Just x
  }


-- | Apply a function to a value.
--
-- >>> import AESD.Types.Value (onDataValue, realValue)
-- >>>
-- >>> let x = realValue 42
-- >>> onDataValue (("real value = " ++) . show) (("integer value = " ++) . show) ("string value = " ++ ) "unknown value" x
-- "real value = 42.0"
onDataValue :: (Double -> a) -- ^ Handle a real value.
            -> (Int64 -> a)  -- ^ Handle an integer value.
            -> (String -> a) -- ^ Handle a string value.
            -> a             -- ^ The default result.
            -> DataValue     -- ^ The data value.
            -> a             -- ^ The result of applying the first applicable function to the value.
onDataValue f g h d x =
  fromMaybe d
     $  g <$> getField (integerValue' x)
    <|> f <$> getField (realValue'    x)
    <|> h <$> getField (stringValue'  x)


-- | Apply a binary function to a pair of values.
onDataValue2 :: (Double -> Double -> a) -- ^ Handle a pair of real values.
             -> (Int64 -> Int64 -> a)   -- ^ Handle a pair of integer values.
             -> (String -> String -> a) -- ^ Handle a pair of string values.
             -> a                       -- ^ The default result.
             -> DataValue               -- ^ The first value.
             -> DataValue               -- ^ The resecond value.
             -> a                       -- ^ The result of applying the first applicable function to the pair of values.
onDataValue2 f g h d x y =
  fromMaybe d
     $  g <$> getField (integerValue' x) <*> getField (integerValue' y)
    <|> f <$> getField (realValue'    x) <*> getField (realValue'    y)
    <|> h <$> getField (stringValue'  x) <*> getField (stringValue'  y)


-- | Apply a function to a list of values.
onDataValues :: ([Double] -> a) -- ^ Handle a list of real values.
             -> ([Int64] -> a)  -- ^ Handle a list of integer values.
             -> ([String] -> a) -- ^ Handle a list of string values.
             -> a               -- ^ The default result.
             -> [DataValue]     -- ^ The data values.
             -> a
onDataValues f g h d xs =
  fromMaybe d
     $  g <$> mapM (getField . integerValue') xs
    <|> f <$> mapM (getField . realValue'   ) xs
    <|> h <$> mapM (getField . stringValue' ) xs


-- | Types of variables.
data VarType =
    RealVar    -- ^ Type for a real value.
  | IntegerVar -- ^ Type for an integer value.
  | StringVar  -- ^ Type for a string value.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)


instance Monoid VarType where
  mempty = StringVar
  mappend IntegerVar IntegerVar = IntegerVar
  mappend StringVar  _          = StringVar
  mappend _          StringVar  = StringVar
  mappend _          _          = RealVar


-- | Get the type of a value.
varType :: DataValue -> Maybe VarType
varType =
  onDataValue
    (Just . const RealVar   )
    (Just . const IntegerVar)
    (Just . const StringVar )
    Nothing


-- | Get the possible types of a value.
varTypes :: DataValue -> [VarType]
varTypes x =
  catMaybes
    [
      const RealVar    <$> getField (realValue'    x)
    , const IntegerVar <$> getField (integerValue' x)
    , const StringVar  <$> getField (stringValue'  x)
    ]


-- | Determine whether a list of values are of the same type.
sameVarTypes :: [DataValue] -> Bool
sameVarTypes xs = length (foldl ((. varTypes) . union) [] xs) == 1


-- | Get type of a value, using the string type as the default.
detectVarType :: DataValue -> VarType
detectVarType x
  | isJust (getField $ integerValue' x) = IntegerVar
  | isJust (getField $ realValue'    x) = RealVar
  | otherwise                           = StringVar


-- | Determine the type of value that two value types can commonly be expressed as.
consistentVarType :: VarType -- ^ The first value type.
                  -> VarType -- ^ The second value type.
                  -> VarType -- ^ The type in which both value types can be commonly expressed.
consistentVarType = mappend


-- | Determine consistent value types.
consistentVarTypes :: [VarType] -- ^ The allowed
                   -> [DataValue] -- ^ The data values.
                   -> [VarType]
consistentVarTypes = zipWith ((. detectVarType) . consistentVarType)


-- | Determine the commonly expressible type of a list of values.
commonVarType :: [DataValue] -> VarType
commonVarType = foldl consistentVarType IntegerVar . fmap detectVarType


-- | Determine the list of commonly expressible types of a table of values.
commonVarTypes :: [[DataValue]] -> [VarType]
commonVarTypes [] = []
commonVarTypes x@(h : _) = foldl consistentVarTypes (const IntegerVar <$> h) x


-- | Cast a value to a particular type.
castVarType :: VarType -> DataValue -> DataValue
castVarType IntegerVar x = x {                        realValue' = mempty, stringValue' = mempty}
castVarType RealVar    x = x {integerValue' = mempty,                      stringValue' = mempty}
castVarType StringVar  x = x {integerValue' = mempty, realValue' = mempty                       }


-- | Cast a list of values to a commmonly expressible type.
castVarTypes :: [DataValue] -> (VarType, [DataValue])
castVarTypes xs =
  let
    t = commonVarType xs
  in
    (t, castVarType t <$> xs)
