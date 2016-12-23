{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Internal (
) where


import Data.ProtocolBuffers (Decode, Encode, decodeMessage, encodeMessage)
import Data.Serialize (runGetLazy, runPutLazy)
import Network.WebSockets (WebSocketsData(..))


instance (Decode a, Encode a) => WebSocketsData a where
  fromLazyByteString = either error id . runGetLazy decodeMessage
  toLazyByteString = runPutLazy . encodeMessage
