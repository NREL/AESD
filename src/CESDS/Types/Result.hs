{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Result (
  ResultIdentifier
, Result(..)
) where


import CESDS.Types (Identifier)
import CESDS.Types.Variable (VariableIdentifier)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)


type ResultIdentifier = Identifier


newtype Result = Result {unResult :: [(VariableIdentifier, Scientific)]}
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
