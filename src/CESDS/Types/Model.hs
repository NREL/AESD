{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}


module CESDS.Types.Model (
  ModelIdentifier
, ModelMeta
, identifier
, name
, uri
, varMeta
, ModelMetas
, models
) where


import CESDS.Types.Internal ()
import CESDS.Types.Variable (VarMeta)
import Control.Lens.Lens (Lens', lens)
import Data.Default (Default(..))
import Data.ProtocolBuffers (Decode, Encode, Message, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


type ModelIdentifier = String


data ModelMeta =
  ModelMeta
  {
    identifier' :: Required 1 (Value   ModelIdentifier)
  , name'       :: Required 2 (Value   String         )
  , uri'        :: Required 3 (Value   String         )
  , varMeta'    :: Repeated 4 (Message VarMeta        )
  }
    deriving (Generic, Show)

instance Default ModelMeta where
  def =
    ModelMeta
    {
      identifier'  = putField ""
    , name'        = putField ""
    , uri'         = putField ""
    , varMeta'     = putField def
    }

instance Decode ModelMeta

instance Encode ModelMeta


identifier :: Lens' ModelMeta String
identifier = lens (getField . identifier') (\s x -> s {identifier' = putField x})


name :: Lens' ModelMeta String
name = lens (getField . name') (\s x -> s {name' = putField x})


uri :: Lens' ModelMeta String
uri = lens (getField . uri') (\s x -> s {uri' = putField x})


varMeta :: Lens' ModelMeta [VarMeta]
varMeta = lens (getField . varMeta') (\s x -> s {varMeta' = putField x})


data ModelMetas =
  ModelMetas
  {
     models' :: Repeated 1 (Message ModelMeta)
  }
    deriving (Generic, Show)

instance Default ModelMetas where
  def = ModelMetas $ putField []

instance Decode ModelMetas

instance Encode ModelMetas


models :: Lens' ModelMetas [ModelMeta]
models = lens (getField . models') (\s x -> s {models' = putField x})
