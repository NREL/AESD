{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module CESDS.Types (
  VersionIdentifier
, OptionalInt32
, int32
, OptionalUInt32
, uint32
, OptionalString
, string
, DataValue
, realValue
, integerValue
, stringValue
, withDataValue
, Doubles
, reals
, Integers
, integers
, Strings
, strings
) where


import CESDS.Types.Internal ()
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Data.Default (Default(..))
import Data.Int (Int32, Int64)
import Data.ProtocolBuffers (Decode, Encode, Optional, Packed, Repeated, Required, Value, getField, putField)
import Data.Word (Word32)
import GHC.Generics (Generic)


type VersionIdentifier = Word32


data OptionalInt32 =
  OptionalInt32
  {
    int32' :: Required 1 (Value Int32)
  }
    deriving (Generic, Show)

instance Default OptionalInt32 where
  def = OptionalInt32 $ putField 0

instance Decode OptionalInt32

instance Encode OptionalInt32


int32 :: Lens' OptionalInt32 Int32
int32 = lens (getField . int32') (\s x -> s {int32' = putField x})


data OptionalUInt32 =
  OptionalUInt32
  {
    uint32' :: Required 1 (Value Word32)
  }
    deriving (Generic, Show)

instance Default OptionalUInt32 where
  def = OptionalUInt32 $ putField 0

instance Decode OptionalUInt32

instance Encode OptionalUInt32


uint32 :: Lens' OptionalUInt32 Word32
uint32 = lens (getField .uint32') (\s x ->  s {uint32' = putField x})


data OptionalString =
  OptionalString
  {
    string' :: Required 1 (Value String)
  }
    deriving (Generic, Show)

instance Default OptionalString where
  def = OptionalString $ putField ""

instance Decode OptionalString

instance Encode OptionalString


string :: Lens' OptionalString String
string = lens (getField . string') (\s x -> s {string' = putField x})


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


withDataValue :: DataValue -> (Double -> a) -> (Int64 -> a) -> (String -> a) -> Maybe a
withDataValue x f g h =
      f <$> x ^. realValue
  <|> g <$> x ^. integerValue
  <|> h <$> x ^. stringValue
    

data Doubles =
  Doubles
  {
    reals' :: Packed 1 (Value Double)
  }
    deriving (Generic, Show)

instance Default Doubles where
  def = Doubles $ putField []

instance Decode Doubles

instance Encode Doubles


reals :: Lens' Doubles [Double]
reals = lens (getField . reals') (\s x -> s {reals' = putField x})


data Integers =
  Integers
  {
    integers' :: Packed 1 (Value Int64)
  }
    deriving (Generic, Show)

instance Default Integers where
  def = Integers $ putField []

instance Decode Integers

instance Encode Integers


integers :: Lens' Integers [Int64]
integers = lens (getField . integers') (\s x -> s {integers' = putField x})


data Strings =
  Strings
  {
    strings' :: Repeated 1 (Value String)
  }
    deriving (Generic, Show)

instance Default Strings where
  def = Strings $ putField []

instance Decode Strings

instance Encode Strings


strings :: Lens' Strings [String]
strings = lens (getField . strings') (\s x -> s {strings' = putField x})
