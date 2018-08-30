{-|
Module      :  $Header$
Copyright   :  (c) 2016-18 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Internal types, mostly for encoding as protocol buffers.
-}


{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module AESD.Types.Internal (
-- * Signed integers
  OptionalInt32
, makeInt32
, int32
-- * Unsigned integers
, OptionalUInt32
, makeUint32
, uint32
-- * Strings
, OptionalString
, makeString
, string
-- * List of values
, Doubles
, makeReals
, reals
, Integers
, makeIntegers
, integers
, Strings
, makeStrings
, strings
) where


import Data.Int (Int32, Int64)
import Data.ProtocolBuffers (Decode, Encode, Packed, Repeated, Required, Value, decodeMessage, encodeMessage, getField, putField)
import Data.Serialize (runGetLazy, runPutLazy)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Network.WebSockets (WebSocketsData(..))


-- | A signed integer.
data OptionalInt32 =
  OptionalInt32
  {
    int32' :: Required 1 (Value Int32)
  }
    deriving (Generic, Show)

instance Decode OptionalInt32

instance Encode OptionalInt32


-- | Make a signed integer.
{-# INLINE makeInt32 #-}
makeInt32 :: Int32 -> OptionalInt32
makeInt32 x = OptionalInt32 {int32' = putField x}


-- | Get a signed integer.
{-# INLINE int32 #-}
int32 :: OptionalInt32 -> Int32
int32 = getField . int32'


-- | An unsigned integer.
data OptionalUInt32 =
  OptionalUInt32
  {
    uint32' :: Required 1 (Value Word32)
  }
    deriving (Generic, Show)

instance Decode OptionalUInt32

instance Encode OptionalUInt32


-- | Make an unsigned integer.
{-# INLINE makeUint32 #-}
makeUint32 :: Word32 -> OptionalUInt32
makeUint32 x = OptionalUInt32 {uint32' = putField x}


-- | Get an unsigned integer.
{-# INLINE uint32 #-}
uint32 :: OptionalUInt32 -> Word32
uint32 = getField .uint32'


-- | A string.
data OptionalString =
  OptionalString
  {
    string' :: Required 1 (Value String)
  }
    deriving (Generic, Show)

instance Decode OptionalString

instance Encode OptionalString


-- | Make a string.
{-# INLINE makeString #-}
makeString :: String -> OptionalString
makeString x = OptionalString {string' = putField x}


-- | Get a string.
{-# INLINE string #-}
string :: OptionalString -> String
string = getField . string'
    

-- | A list of real numbers.
data Doubles =
  Doubles
  {
    reals' :: Packed 1 (Value Double)
  }
    deriving (Generic, Show)

instance Decode Doubles

instance Encode Doubles


-- | Make a list of real numbers.
{-# INLINE makeReals #-}
makeReals :: [Double] -> Doubles
makeReals x = Doubles {reals' = putField x}


-- | Get a list of real numbers.
{-# INLINE reals #-}
reals :: Doubles -> [Double]
reals = getField . reals'


-- | A list of integers.
data Integers =
  Integers
  {
    integers' :: Packed 1 (Value Int64)
  }
    deriving (Generic, Show)

instance Decode Integers

instance Encode Integers


-- | Make a list of integers.
{-# INLINE makeIntegers #-}
makeIntegers :: [Int64] -> Integers
makeIntegers x = Integers {integers' = putField x}


-- | Get a list of integers.
{-# INLINE integers #-}
integers :: Integers -> [Int64]
integers = getField . integers'


-- | A list of strings.
data Strings =
  Strings
  {
    strings' :: Repeated 1 (Value String)
  }
    deriving (Generic, Show)

instance Decode Strings

instance Encode Strings


-- | Make a list of strings.
{-# INLINE makeStrings #-}
makeStrings :: [String] -> Strings
makeStrings x = Strings {strings' = putField x}


-- | Get a list of strings.
{-# INLINE strings #-}
strings :: Strings -> [String]
strings = getField . strings'


-- Enable protocol buffers for transport via web sockets.
instance (Decode a, Encode a) => WebSocketsData a where
  fromLazyByteString = either error id . runGetLazy decodeMessage
  toLazyByteString = runPutLazy . encodeMessage
