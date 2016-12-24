{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module CESDS.Types (
  VersionIdentifier
, DataValue
, realValue
, integerValue
, stringValue
, onDataValue
) where


import CESDS.Types.Internal ()
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Data.Default (Default(..))
import Data.Int (Int64)
import Data.ProtocolBuffers (Decode, Encode, Optional, Value, getField, putField)
import Data.Word (Word32)
import GHC.Generics (Generic)


type VersionIdentifier = Word32


data DataValue =
  DataValue
  {
    realValue'    :: Optional 1 (Value Double)
  , integerValue' :: Optional 2 (Value Int64 )
  , stringValue'  :: Optional 3 (Value String)
  }
    deriving (Generic, Show)

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
