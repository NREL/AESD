{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for model metadata.
-}


{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}


module AESD.Types.Model (
-- * Model metadata
  ModelIdentifier
, ModelMeta
, makeModelMeta
, identifier
, name
, uri
, varMeta
, inputs
-- * Collections of model metadata
, ModelMetas
, makeModels
, models
) where


import AESD.Types.Domain (DomainMeta)
import AESD.Types.Internal ()
import AESD.Types.Variable (VarMeta)
import Data.ProtocolBuffers (Decode, Encode, Message, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


-- | A unique identifier for a model.
type ModelIdentifier = String


-- | Model metadata.
data ModelMeta =
  ModelMeta
  {
    identifier' :: Required 1 (Value   ModelIdentifier)
  , name'       :: Required 2 (Value   String         )
  , uri'        :: Required 3 (Value   String         )
  , varMeta'    :: Repeated 4 (Message VarMeta        )
  , inputs'     :: Repeated 5 (Message DomainMeta     )
  }
    deriving (Generic, Show)

instance Decode ModelMeta

instance Encode ModelMeta


-- | Construct model metadata.
{-# INLINE makeModelMeta #-}
makeModelMeta :: ModelIdentifier -- ^ The unique identifier for the model.
              -> String          -- ^ The name of the model.
              -> String          -- ^ The URI for the model.
              -> [VarMeta]       -- ^ The metadata for the variables in the model.
              -> [DomainMeta]    -- ^ The metadata for the domains of the model's input variables, if any.
              -> ModelMeta       -- ^ The model metadata.
makeModelMeta identifier'' name'' uri'' varMeta'' inputs'' =
    ModelMeta
    {
      identifier'  = putField identifier''
    , name'        = putField name''
    , uri'         = putField uri''
    , varMeta'     = putField varMeta''
    , inputs'      = putField inputs''
    }


-- | Get the model's identifier.
{-# INLINE identifier #-}
identifier :: ModelMeta -> String
identifier = getField . identifier'


-- | Get the model's name.
{-# INLINE name #-}
name :: ModelMeta -> String
name = getField . name'


-- | Get the URI for the model.
{-# INLINE uri #-}
uri :: ModelMeta -> String
uri = getField . uri'


-- | Get the metadata for the variables in the model.
{-# INLINE varMeta #-}
varMeta :: ModelMeta -> [VarMeta]
varMeta = getField . varMeta'


-- | Get the metadata for the model's input variables, if any.
{-# INLINE inputs #-}
inputs :: ModelMeta -> [DomainMeta]
inputs = getField . inputs'


-- | A collection of model metadata.
data ModelMetas =
  ModelMetas
  {
     models' :: Repeated 1 (Message ModelMeta)
  }
    deriving (Generic, Show)

instance Decode ModelMetas

instance Encode ModelMetas


-- | Make a collection of model metadata.
{-# INLINE makeModels #-}
makeModels :: [ModelMeta] -> ModelMetas
makeModels models'' =
  ModelMetas
  {
    models' = putField models''
  }


-- | Get the list of model metadata from a collection.
{-# INLINE models #-}
models :: ModelMetas -> [ModelMeta]
models = getField . models'
