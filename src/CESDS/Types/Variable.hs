{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Variable (
  VariableIdentifier
, VarMeta
, identifier
, name
, units
, varType
, VarUnits(..)
) where


import CESDS.Types.Internal ()
import CESDS.Types.Value (VarType(RealVar))
import Control.Lens.Lens (Lens', lens)
import Data.Default (Default(..))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Enumeration, Optional, Packed, Required, Value, getField, putField)
import GHC.Generics (Generic)


type VariableIdentifier = Int32


data VarMeta =
  VarMeta
  {
    identifier' :: Optional 1 (Value       VariableIdentifier)
  , name'       :: Required 2 (Value       String            )
  , isInput'    :: Optional 3 (Value       Bool              ) -- FIXME: Delete this in next version of API.
  , units'      :: Optional 4 (Value       String            )
  , si'         :: Packed   5 (Value       Int32             )
  , scale'      :: Required 6 (Value       Double            )
  , varType'    :: Optional 7 (Enumeration VarType           )
  }
    deriving (Generic, Show)

instance Default VarMeta where
  def =
    VarMeta
    {
      identifier' = putField $ Just 0
    , name'       = putField ""
    , isInput'    = putField Nothing
    , units'      = putField Nothing
    , si'         = putField $ replicate 8 0
    , scale'      = putField 1
    , varType'    = putField $ Just RealVar
    }

instance Decode VarMeta

instance Encode VarMeta


identifier :: Lens' VarMeta Int32
identifier = lens (fromMaybe 0 . getField . identifier') (\s x -> s {identifier' = putField $ Just x})


name :: Lens' VarMeta String
name = lens (getField . name') (\s x -> s {name' = putField x})


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
          si'    = putField [lengthExponent, massExponent, timeExponent, currentExponent, temperatureExponent, molExponent, intensityExponent, angleExponent]
        , scale' = putField scale
        }
    )


varType :: Lens' VarMeta VarType
varType = lens (fromMaybe RealVar . getField . varType') (\s x -> s {varType' = putField $ Just x})


data VarUnits =
  VarUnits
  {
    unitName            :: Maybe String
  , lengthExponent      :: Int32
  , massExponent        :: Int32
  , timeExponent        :: Int32
  , currentExponent     :: Int32
  , temperatureExponent :: Int32
  , molExponent         :: Int32
  , intensityExponent   :: Int32
  , angleExponent       :: Int32
  , scale               :: Double
  }
    deriving (Eq, Generic, Ord, Show)

instance Default VarUnits where
  def = VarUnits Nothing 0 0 0 0 0 0 0 0 1
