{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Record (
  RecordIdentifier
, Record(..)
) where


import CESDS.Types (Identifier)
import CESDS.Types.Variable (VariableIdentifier)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)


type RecordIdentifier = Identifier


newtype Record = Record {unRecord :: [(VariableIdentifier, Scientific)]}
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
