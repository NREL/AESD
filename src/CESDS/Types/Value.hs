{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module CESDS.Types.Value (
  DataValue
, realValue
, integerValue
, stringValue
, onDataValue
, VarType(..)
, detectVarType
, commonVarType
, sameVarTypes
) where


import CESDS.Types.Internal ()
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Int (Int64)
import Data.Maybe (isJust)
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


onDataValue :: (Double -> a) -> (Int64 -> a) -> (String -> a) -> DataValue -> Maybe a
onDataValue f g h x =
      f <$> x ^. realValue
  <|> g <$> x ^. integerValue
  <|> h <$> x ^. stringValue


data VarType = RealVar | IntegerVar | StringVar
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)


detectVarType :: DataValue -> VarType
detectVarType x
  | isJust (x ^. integerValue) = IntegerVar
  | isJust (x ^. realValue   ) = RealVar
  | otherwise                  = StringVar


commonVarType :: [DataValue] -> VarType
commonVarType =
  foldl f RealVar . fmap detectVarType
    where
      f IntegerVar IntegerVar = IntegerVar
      f StringVar  _          = StringVar
      f _          StringVar  = StringVar
      f _          _          = RealVar


castVarType :: VarType -> DataValue -> DataValue
castVarType IntegerVar =                             (realValue .~ Nothing) . (stringValue .~ Nothing)
castVarType RealVar    = (integerValue .~ Nothing)                          . (stringValue .~ Nothing)
castVarType StringVar  = (integerValue .~ Nothing) . (realValue .~ Nothing)


sameVarTypes :: [DataValue] -> (VarType, [DataValue])
sameVarTypes xs =
  let
    t = commonVarType xs
  in
    (t, castVarType t <$> xs)
