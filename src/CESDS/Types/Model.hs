{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Model (
  ModelIdentifier
, Model(..)
) where


import CESDS.Types (Generation, Identifier, Tags)
import CESDS.Types.Variable (VariableIdentifier)
import Data.Aeson (Value(String))
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), typeMismatch)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI)


type ModelIdentifier = Identifier


data Model =
  Model
  {
    modelID          :: ModelIdentifier
  , modelURI         :: URI
  , modelName        :: Maybe Text
  , modelDescription :: Maybe Text
  , modelTags        :: Tags
  , generation       :: Generation
  , variables        :: [VariableIdentifier]
  , primaryKey       :: VariableIdentifier
  , timeKey          :: Maybe VariableIdentifier
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


instance Read URI where
  readsPrec _ s = case parseURI s of
                    Nothing  -> []
                    Just uri -> [(uri, "")]

instance FromJSON URI where
  parseJSON invalid@(String s) = maybe (typeMismatch "URI" invalid) return . parseURI $ unpack s
  parseJSON invalid            = typeMismatch "URI" invalid

instance ToJSON URI where
  toJSON = String . pack . show
