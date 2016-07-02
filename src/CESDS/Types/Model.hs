{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Model (
  ModelIdentifier
, Model(..)
) where


import CESDS.Types (Generation, Identifier, Tags, object')
import CESDS.Types.Variable (Variable, VariableIdentifier)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI (URI)


type ModelIdentifier = Identifier


data Model =
  Model
  {
    identifier  :: ModelIdentifier
  , uri         :: URI
  , name        :: Maybe Text
  , description :: Maybe Text
  , tags        :: Tags
  , generation  :: Generation
  , variables   :: [Variable]
  , primaryKey  :: VariableIdentifier
  , timeKey     :: Maybe VariableIdentifier
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Model where
  parseJSON = withObject "MODEL" $ \o ->
                do
                  identifier  <- o .:  "model_id"
                  uri         <- o .:  "model_uri"
                  name        <- o .:? "label"
                  description <- o .:? "description"
                  tags        <- o .:  "tags"
                  generation  <- o .:  "generation"
                  variables   <- o .:  "variables"
                  primaryKey  <- o .:  "primary_key"
                  timeKey     <- o .:? "time_key"
                  return Model{..}

instance ToJSON Model where
  toJSON Model{..} = object'
                       [
                         "model_id"    .= identifier
                       , "model_uri"   .= uri
                       , "label"       .= name
                       , "description" .= description
                       , "tags"        .= tags
                       , "generation"  .= generation
                       , "variables"   .= variables
                       , "primary_key" .= primaryKey
                       , "time_key"    .= timeKey
                       ]
