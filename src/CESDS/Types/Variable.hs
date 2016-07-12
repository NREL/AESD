{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Variable (
  VariableIdentifier
, Variable(..)
, validateVariable
, hasVariable
, canHaveVal
, Display(..)
, Domain(..)
, validateDomain
, isSet
, SetValue
, compatibleDomains
, Units(..)
) where


import CESDS.Types (Color, Identifier, Val(..), object')
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.!=), (.=), withObject)
import Data.List (find)
import Data.List.Util (notDuplicatedIn, sameElements)
import Data.Scientific (Scientific)
import Data.String (IsString)
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
        identifier <- o .:  "var_id"
        display    <- o .:  "display"
        domain     <- o .:  "domain"
        units      <- o .:? "units"
        isInput    <- o .:  "is_input" .!= False
        return Variable{..}

instance ToJSON Variable where
  toJSON Variable{..} =
    object'
      [
        "var_id"   .= identifier
      , "disp"     .= display
      , "domain"   .= domain
      , "units"    .= units
      , "is_input" .= isInput
      ]


validateVariable :: (IsString e, MonadError e m) => [VariableIdentifier] -> Variable -> m ()
validateVariable variableIdentifiers Variable{..} =
  do
    assert "duplicate variable identifiers" $ notDuplicatedIn id identifier variableIdentifiers
    validateDomain domain


hasVariable :: (IsString e, MonadError e m) => [Variable] -> VariableIdentifier -> m Variable
hasVariable variables variableIdentifier =
  maybe
    (throwError "missing variable")
    return
    $ find ((== variableIdentifier) . identifier) variables


canHaveVal :: (IsString e, MonadError e m) => Domain -> Val -> m ()
canHaveVal (Interval Nothing  Nothing ) (Continuous _) = return ()
canHaveVal (Interval Nothing  (Just u)) (Continuous x) = assert "value not in domain" $           x <= u
canHaveVal (Interval (Just l) Nothing ) (Continuous x) = assert "value not in domain" $ l <= x          
canHaveVal (Interval (Just l) (Just u)) (Continuous x) = assert "value not in domain" $ l <= x && x <= u
canHaveVal (Set []                    ) (Discrete   _) = return ()
canHaveVal (Set ys                    ) (Discrete   x) = assert "value incompatible with domain" $ x `elem` ys
canHaveVal _                            _              = throwError "value not in domain"


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
        label      <- o .:  "label"
        shortLabel <- o .:? "shortlabel"
        color      <- o .:? "color"
        return Display{..}

instance ToJSON Display where
  toJSON Display{..} =
    object'
      [
        "label"      .= label
      , "shortlabel" .= shortLabel
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
      options :: [SetValue]
    }
    deriving (Generic, Read, Show)

instance Eq Domain where
  Interval xl xu == Interval yl yu = xl == yl && xu == yu
  Set x          == Set y          = x `sameElements` y
  _              == _              = False

instance FromJSON Domain where
  parseJSON =
    withObject "VAR domain" $ \o ->
      (parseInterval =<< o .: "interval") <|> (parseSet =<< o .: "set")
    where
      parseInterval =
        withObject "VAR domain interval" $ \o' ->
          do
            bounds <- o' .: "bounds"
            when (length bounds /= 2)
              $ fail "VAR domain interval bounds must contain two entries"
            let
              [lowerBound, upperBound] = bounds
            return Interval{..}
      parseSet =
        withObject "VAR domain set" $ \o' ->
          do
            options <- o' .:? "options" .!= []
            return Set{..}

instance ToJSON Domain where
  toJSON Interval{..} = object' ["interval" .= object' ["bounds"  .= [lowerBound, upperBound]]]
  toJSON Set{..}      = object' ["set"      .= object' ["options" .= options                 ]]


validateDomain :: (IsString e, MonadError e m) => Domain -> m ()
validateDomain (Interval (Just l) (Just u)) = assert "lower bound greater than upper bound" $ l <= u
validateDomain _                            = return ()


isSet :: Domain -> Bool
isSet Set{} = True
isSet _     = False


type SetValue = Text


compatibleDomains :: (IsString e, MonadError e m) => Domain -> Domain -> m ()
compatibleDomains Interval{} Interval{} = return ()
compatibleDomains Set{}      Set{}      = return ()
compatibleDomains _          _          = throwError "incompatible domains"


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
        when (length si /= 8)
          $ fail "VAR units SI must contain eight entries"
        let
          [lengthExponent, massExponent, timeExponent, currentExponent, temperatureExponent, molExponent, intensityExponent, angleExponent] = si
        scale <- o .:? "scale" .!= 1
        return Units{..}

instance ToJSON Units where
  toJSON Units{..} =
    object'
      [
        "SI"    .= [lengthExponent, massExponent, timeExponent, currentExponent, temperatureExponent, molExponent, intensityExponent, angleExponent]
      , "scale" .= scale
      ]
