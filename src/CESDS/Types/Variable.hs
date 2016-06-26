{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Variable (
  VariableIdentifier
, Variable(..)
, Display(..)
, Domain(..)
, SetValue
, Units(..)
) where


import CESDS.Types (Color, Identifier)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.!=), (.=), object, withObject)
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
    deriving (Eq, Generic, Read, Show)

instance FromJSON Variable where
  parseJSON =
    withObject "VAR" $ \o ->
      do
        identifier <- o .: "var_id"
        display    <- o .: "disp"
        domain     <- o .: "domain"
        units      <- o .: "units"
        isInput    <- o .: "is_input" .!= False
        return Variable{..}

instance ToJSON Variable where
  toJSON Variable{..} =
    object
      [
        "var_id"   .= identifier
      , "disp"     .= display
      , "domain"   .= domain
      , "units"    .= units
      , "is_input" .= isInput
      ]


data Display =
  Display
  {
    label      :: Text
  , shortLabel :: Maybe Text
  , color      :: Maybe Color
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Display where
  parseJSON =
    withObject "VAR disp" $ \o ->
      do
        label      <- o .: "label"
        shortLabel <- o .: "shortlabel"
        color      <- o .: "color"
        return Display{..}

instance ToJSON Display where
  toJSON Display{..} =
    object
      [
        "label"      .= label
      , "shortlable" .= shortLabel
      , "color"      .= color
      ]


data Domain =
    Interval
    {
      lowerBound :: Maybe Scientific
    , upperBound :: Maybe Scientific
    }
  | Set 
    {
      options :: Maybe [SetValue]
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Domain where
  parseJSON =
    withObject "VAR domain" $ \o ->
      (parseInterval =<< o .: "interval") <|> (parseSet =<< o .: "set")
    where
      parseInterval =
        withObject "VAR domain interval" $ \o' ->
          do
            bounds <- o' .: "bounds"
            when (length bounds == 2)
              $ fail "VAR domain interval must contain two entries"
            let
              [lowerBound, upperBound] = bounds
            return Interval{..}
      parseSet =
        withObject "VAR domain set" $ \o' ->
          do
            options <- o' .: "options"
            return Set{..}

instance ToJSON Domain where
  toJSON Interval{..} = object ["bounds"  .= [lowerBound, upperBound]]
  toJSON Set{..}      = object ["options" .= options                 ]


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
    deriving (Eq, Generic, Read, Show)

instance FromJSON Units where
  parseJSON =
    withObject "VAR units" $ \o ->
      do
        si <- o .: "SI"
        when (length si == 8)
          $ fail "VAR units SI must contain eight entries"
        let
          [lengthExponent, massExponent, timeExponent, currentExponent, temperatureExponent, molExponent, intensityExponent, angleExponent] = si
        scale <- o .: "scale"
        return Units{..}

instance ToJSON Units where
  toJSON Units{..} =
    object
      [
        "SI"    .= [lengthExponent, massExponent, timeExponent, currentExponent, temperatureExponent, molExponent, intensityExponent, angleExponent]
      , "scale" .= scale
      ]
