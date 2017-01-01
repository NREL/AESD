{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module NREL.Meters (
  Site(..)
, meters
, siteModels
) where


import CESDS.Haystack (HaystackAccess)
import CESDS.Types.Model as Model (ModelMeta, identifier, name, uri, varMeta)
import CESDS.Types.Value as Value (VarType(..))
import CESDS.Types.Variable as Variable (VarUnits(VarUnits), identifier, name, units, varType)
import Control.Arrow ((***))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Pair, Value(String), object, withObject, withText)
import Data.Default (def)
import Data.HashMap.Strict (toList)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI)

import qualified Data.Text as T (cons, pack, splitAt, tail, unpack)


newtype Tags = Tags {unTags :: [Pair]}
  deriving (Generic, Read, Show)

instance Eq Tags where
  Tags x == Tags y = object x == object y

instance FromJSON Tags where
  parseJSON =
    withObject "tags" $ return . Tags . toList

instance ToJSON Tags where
  toJSON = object . unTags


data Site  =
  Site
  {
    siteAccess      :: HaystackAccess
  , siteIdentifier  :: Text
  , siteURI         :: URI
  , siteName        :: Text
  , siteDescription :: Text
  , siteTags        :: Tags
  , siteMeters      :: [Text]
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Site

instance ToJSON Site


instance Read URI where
  readsPrec _ s = case parseURI s of
                    Nothing -> []
                    Just u  -> [(u, "")]

instance FromJSON URI where
  parseJSON = withText "URI" $ return . read . T.unpack

instance ToJSON URI where
  toJSON = String . T.pack . show


meters :: Site -> [(Text, Text)]
meters Site{..} = map (swap . (T.cons '@' *** T.tail) . T.splitAt 17) siteMeters


siteModels :: Site -> [ModelMeta]
siteModels site@Site{..} =
  [
    def
      & Model.identifier .~ identifier'
      & Model.name       .~ label'
      & Model.uri        .~ show siteURI ++ "#" ++ identifier'
      & Model.varMeta    .~ [
                              def
                                & Variable.identifier .~ 0
                                & Variable.name       .~ "Time"
                                & Variable.varType    .~ StringVar
                            , def
                                & Variable.identifier .~ 1
                                & Variable.name       .~ "Epoch"
                                & Variable.units      .~ VarUnits (Just "POSIX Seconds") 0 0 1 0 0 0 0 0 1
                                & Variable.varType    .~ IntegerVar
                            , def
                                & Variable.identifier .~ 2
                                & Variable.name       .~ "Measurement"
                                & Variable.varType    .~ RealVar
                            ]
  |
    (label', identifier') <- (T.unpack *** T.unpack) <$> meters site
  ]
