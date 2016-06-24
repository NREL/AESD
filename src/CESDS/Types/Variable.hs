{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Variable (
  VariableIdentifier
, Variable(..)
, Display(..)
, Domain(..)
, SetValue
, Units(..)
) where


import CESDS.Types (Color, Identifier)
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)


type VariableIdentifier = Identifier


data Variable =
  Variable
  {
    identifier :: VariableIdentifier
  , display    :: Display
  , domain     :: Domain
  , units      :: Maybe Units
  , isInput    :: Bool
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


data Display =
  Display
  {
    label      :: Text
  , shortLabel :: Maybe Text
  , color      :: Maybe Color
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


data Domain =
    Interval
    {
      lowerBound :: Maybe Scientific
    , upperBound :: Maybe Scientific
    }
  | Set 
    {
      options :: [SetValue]
    }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


type SetValue = Text


data Units =
  Units
  {
    lengthExponent      :: Int
  , massExponent        :: Int
  , timeExponent        :: Int
  , currentExponent     :: Int
  , temperatureExponent :: Int
  , molExponent         :: Int
  , intensityExponent   :: Int
  , angleExponent       :: Int
  , scale               :: Scientific
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
