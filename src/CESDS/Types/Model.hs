{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Model (
  ModelIdentifier
, Model(..)
) where


import CESDS.Types (Color, Generation, Identifier, Tags)
import CESDS.Types.Variable (Domain, VariableIdentifier)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI (URI)


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
