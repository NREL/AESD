{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module CESDS.Types.Value (
  DataValue
, realValue
, integerValue
, stringValue
, onDataValue
, onDataValues
, VarType(..)
, varType
, varTypes
, sameVarTypes
, detectVarType
, consistentVarType
, consistentVarTypes
, castVarType
, castVarTypes
) where


import CESDS.Types.Internal ()
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Int (Int64)
import Data.List (union)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.ProtocolBuffers (Decode, Encode, Optional, Value, getField, putField)
import GHC.Generics (Generic)
import Text.Read (readMaybe)


data DataValue =
  DataValue
  {
    realValue'    :: Optional 1 (Value Double)
  , integerValue' :: Optional 2 (Value Int64 )
  , stringValue'  :: Optional 3 (Value String)
  }
    deriving (Generic, Show)


instance Eq DataValue where
  (==) = onDataValues (==) (==) (==) False

instance Ord DataValue where
  compare x y = onDataValues compare compare compare (compare x y) x y

instance Read DataValue where
  readsPrec _ x =
    [
      (
        def
          & integerValue .~ readMaybe x
          & realValue    .~ readMaybe x
          & stringValue  .~ Just      x
      , ""
      )
    ]

instance Default DataValue where
  def = DataValue (putField Nothing) (putField Nothing) (putField Nothing)

instance Decode DataValue

instance Encode DataValue


realValue :: Lens' DataValue (Maybe Double)
realValue = lens (getField . realValue') (\s x -> s {realValue' = putField x})


integerValue :: Lens' DataValue (Maybe Int64)
integerValue = lens (getField . integerValue') (\s x -> s {integerValue' = putField x})


stringValue :: Lens' DataValue (Maybe String)
stringValue = lens (getField . stringValue') (\s x -> s {stringValue' = putField x})


onDataValue :: (Double -> a) -> (Int64 -> a) -> (String -> a) -> a -> DataValue -> a
onDataValue f g h d x =
  fromMaybe d
     $  g <$> x ^. integerValue
    <|> f <$> x ^. realValue
    <|> h <$> x ^. stringValue


onDataValues :: (Double -> Double -> a) -> (Int64 -> Int64 -> a) -> (String -> String -> a) -> a -> DataValue -> DataValue -> a
onDataValues f g h d x y =
  fromMaybe d
     $  g <$> x ^. integerValue <*> y ^. integerValue
    <|> f <$> x ^. realValue    <*> y ^. realValue
    <|> h <$> x ^. stringValue  <*> y ^. stringValue


data VarType = RealVar | IntegerVar | StringVar
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)


varType :: DataValue -> Maybe VarType
varType = onDataValue (Just . const RealVar) (Just . const IntegerVar) (Just . const StringVar) Nothing


varTypes :: DataValue -> [VarType]
varTypes x =
  catMaybes
    [
      const RealVar    <$> x ^. realValue
    , const IntegerVar <$> x ^. integerValue
    , const StringVar  <$> x ^. stringValue
    ]


sameVarTypes :: [DataValue] -> Bool
sameVarTypes xs = length (foldl ((. varTypes) . union) [] xs) == 1


detectVarType :: DataValue -> VarType
detectVarType x
  | isJust (x ^. integerValue) = IntegerVar
  | isJust (x ^. realValue   ) = RealVar
  | otherwise                  = StringVar


consistentVarType :: VarType -> VarType -> VarType
consistentVarType IntegerVar IntegerVar = IntegerVar
consistentVarType StringVar  _          = StringVar
consistentVarType _          StringVar  = StringVar
consistentVarType _          _          = RealVar


consistentVarTypes :: [VarType] -> [DataValue] -> [VarType]
consistentVarTypes = zipWith ((. detectVarType) . consistentVarType)


castVarType :: VarType -> DataValue -> DataValue
castVarType IntegerVar =                             (realValue .~ Nothing) . (stringValue .~ Nothing)
castVarType RealVar    = (integerValue .~ Nothing)                          . (stringValue .~ Nothing)
castVarType StringVar  = (integerValue .~ Nothing) . (realValue .~ Nothing)


commonVarType :: [DataValue] -> VarType
commonVarType =
  foldl consistentVarType IntegerVar . fmap detectVarType


castVarTypes :: [DataValue] -> (VarType, [DataValue])
castVarTypes xs =
  let
    t = commonVarType xs
  in
    (t, castVarType t <$> xs)
