{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types (
  Identifier
, Color
, Tags(..)
, Generation
, Val(..)
, valAsString
, isContinuous
, isDiscrete
, object'
) where


import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Pair, Value(Null, Number, String), object, withObject, withText)
import Data.Colour.SRGB (Colour, sRGB24reads, sRGB24shows)
import Data.HashMap.Strict (toList)
import Data.Scientific (Scientific)
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


newtype Tags = Tags {unTags :: [Pair]}
  deriving (Generic, Read, Show)

instance Eq Tags where
  Tags x == Tags y = object x == object y

instance FromJSON Tags where
  parseJSON =
    withObject "tags" $ return . Tags . toList

instance ToJSON Tags where
  toJSON = object . unTags


type Generation = Int


instance Read URI where
  readsPrec _ s = case parseURI s of
                    Nothing  -> []
                    Just uri -> [(uri, "")]

instance FromJSON URI where
  parseJSON = withText "URI" $ return . read . unpack

instance ToJSON URI where
  toJSON = String . pack . show


data Val =
    Continuous Scientific
  | Discrete   Text
    deriving (Eq, Read, Show)

instance FromJSON Val where
  parseJSON (Number x) = return $ Continuous x
  parseJSON (String x) = return $ Discrete   x
  parseJSON x          = fail $ "invalid value \"" ++ show x ++ "\""

instance ToJSON Val where
  toJSON (Continuous x) = Number x
  toJSON (Discrete   x) = String x


valAsString :: Val -> String
valAsString (Continuous x) = show x
valAsString (Discrete   x) = unpack x


isContinuous :: Val -> Bool
isContinuous (Continuous _) = True
isContinuous _              = False


isDiscrete :: Val -> Bool
isDiscrete (Discrete _ ) = True
isDiscrete _             = False


object' :: [Pair] -> Value
object' = object . filter ((/= Null) . snd)
