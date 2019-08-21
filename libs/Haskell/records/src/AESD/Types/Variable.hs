{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for variables.

For example, one can create variable metadata as follows:

>>> import AESD.Types.Value (VarType(..))
>>> import AESD.Types.Variable (VarUnits(..), makeVarMeta, units, varType)
>>>
>>> let x = makeVarMeta 0 "a variable" & varType .~ IntegerVar & units .~ def {unitName = Just "hour", timeExponent = 1, scale = 3600}
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}


module AESD.Types.Variable (
-- * Variables
  VariableIdentifier
, VarMeta
, makeVarMeta
, identifier
, name
, units
, varType
-- * Units of measure
, VarUnits(..)
) where


import AESD.Types.Internal ()
import AESD.Types.Value (VarType(RealVar))
import Control.Lens.Lens (Lens', lens)
import Data.Default (Default(def))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Enumeration, Optional, Packed, Required, Value, getField, putField)
import GHC.Generics (Generic)


-- | A unique identifier for a variable.
type VariableIdentifier = Int32


-- | Metadata for a variable.
data VarMeta =
  VarMeta
  {
    identifier' :: Optional 1 (Value       VariableIdentifier)
  , name'       :: Required 2 (Value       String            )
  , units'      :: Optional 3 (Value       String            )
  , si'         :: Packed   4 (Value       Int32             )
  , scale'      :: Required 5 (Value       Double            )
  , varType'    :: Optional 6 (Enumeration VarType           )
  }
    deriving (Generic, Show)

instance Decode VarMeta

instance Encode VarMeta


-- | Construct a variable.  By default it is unitless and real.
makeVarMeta :: VariableIdentifier -- ^ The unique identifier for the variable.
            -> String             -- ^ The name of the variable.
            -> VarMeta            -- ^ The variable's metadata.
makeVarMeta identifier'' name'' =
  VarMeta
  {
    identifier' = putField $ Just identifier''
  , name'       = putField name''
  , units'      = mempty
  , si'         = putField $ replicate 8 0
  , scale'      = putField 1
  , varType'    = putField $ Just RealVar
  }


-- | Get the unique identifier for a variable.
identifier :: VarMeta -> VariableIdentifier
identifier = fromMaybe 0 . getField . identifier'


-- | Get the name of a variable.
name :: VarMeta -> String
name = getField . name'


-- | Lens for the units of a variable.
units :: Lens' VarMeta VarUnits
units =
  lens
    (
      \VarMeta{..} ->
        let
          unitName = getField units'
          [lengthExponent, massExponent, timeExponent, currentExponent, temperatureExponent, molExponent, intensityExponent, angleExponent] = getField si'
          scale = getField scale'
        in
          VarUnits{..}
    )
    (
      \s VarUnits{..} ->
        s
        {
          units' = putField unitName
        , si'    = putField [lengthExponent, massExponent, timeExponent, currentExponent, temperatureExponent, molExponent, intensityExponent, angleExponent]
        , scale' = putField scale
        }
    )


-- | Lens for the type of a variable.
varType :: Lens' VarMeta VarType
varType = lens (fromMaybe RealVar . getField . varType') (\s x -> s {varType' = putField $ Just x})


-- | Units of measure for a variable.
data VarUnits =
  VarUnits
  {
    unitName            :: Maybe String -- ^ The name of the unit.
  , lengthExponent      :: Int32        -- ^ The exponent of meters.
  , massExponent        :: Int32        -- ^ The exponent of kilograms.
  , timeExponent        :: Int32        -- ^ The exponent of seconds.
  , currentExponent     :: Int32        -- ^ The exponent of amperes.
  , temperatureExponent :: Int32        -- ^ The exponent of kelvins.
  , molExponent         :: Int32        -- ^ The exponent of moles.
  , intensityExponent   :: Int32        -- ^ The exponent of calendas.
  , angleExponent       :: Int32        -- ^ The exponent of radians.
  , scale               :: Double       -- ^ The scale factor.
  }
    deriving (Eq, Generic, Ord, Show)

instance Default VarUnits where
  def = VarUnits Nothing 0 0 0 0 0 0 0 0 1
