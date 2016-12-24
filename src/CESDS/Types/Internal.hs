{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Internal (
  OptionalInt32
, int32
, OptionalUInt32
, uint32
, OptionalString
, string
, Doubles
, reals
, Integers
, integers
, Strings
, strings
) where


import Control.Lens.Lens (Lens', lens)
import Data.Default (Default(..))
import Data.Int (Int32, Int64)
import Data.ProtocolBuffers (Decode, Encode, Packed, Repeated, Required, Value, decodeMessage, encodeMessage, getField, putField)
import Data.Serialize (runGetLazy, runPutLazy)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Network.WebSockets (WebSocketsData(..))


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


instance (Decode a, Encode a) => WebSocketsData a where
  fromLazyByteString = either error id . runGetLazy decodeMessage
  toLazyByteString = runPutLazy . encodeMessage
