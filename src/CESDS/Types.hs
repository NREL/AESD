{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types (
  Identifier
, Color
, Tags(..)
, Generation
) where


import Data.Aeson (Value(String))
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), withText)
import Data.Colour.SRGB (Colour, sRGB24reads, sRGB24shows)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI)


type Identifier = Text


type Color = Colour Double

instance Read Color where
  readsPrec _ = sRGB24reads

instance Show Color where
  showsPrec _ = sRGB24shows

instance FromJSON Color where
  parseJSON = withText "COLOR" $ return . read . unpack

instance ToJSON Color where
  toJSON = String . pack . show


newtype Tags = Tags {unTags :: [(Identifier, Value)]}
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


type Generation = Int


instance Read URI where
  readsPrec _ s = case parseURI s of
                    Nothing  -> []
                    Just uri -> [(uri, "")]

instance FromJSON URI where
  parseJSON = withText "URI" $ return . read . unpack

instance ToJSON URI where
  toJSON = String . pack . show
