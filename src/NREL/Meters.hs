{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module NREL.Meters (
  Site(..)
, meters
, siteModel
, siteModels
) where


import CESDS.Haystack (HaystackAccess)
import CESDS.Types (Tags(..))
import CESDS.Types.Model as M (Model(..))
import CESDS.Types.Variable as V (Display(..), Domain(..), Units(..), Variable(..), VariableIdentifier)
import Control.Arrow ((***))
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Network.URI (URI)

import qualified Data.Text as T (cons, splitAt, tail)


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

instance FromJSON Site where

instance ToJSON Site where


meters :: Site -> [(Text, VariableIdentifier)]
meters Site{..} = map (swap . (T.cons '@' *** T.tail) . T.splitAt 17) siteMeters


siteModel :: Site -> Model
siteModel site@Site{..} =
  let
    identifier  = siteIdentifier
    uri         = Just siteURI
    name        = siteName
    description = Just siteDescription
    tags        = Just siteTags
    generation  = 0
    recordCount = 0
    variables   = [
                    Variable
                    {
                      V.identifier = "time"
                    , display      = Display
                                     {
                                       label      = "Time Stamp"
                                     , shortLabel = Just "Time"
                                     , color      = Nothing
                                     }
                    , domain       = Set []
                    , units        = Nothing
                    , isInput      = False
                    }
                  , Variable
                    {
                      V.identifier = "epoch"
                    , display      = Display
                                     {
                                       label      = "POSIX Seconds"
                                     , shortLabel = Just "Seconds"
                                     , color      = Nothing
                                     }
                    , domain       = Interval (Just 315558000) Nothing
                    , units        = Just $ Units 0 0 1 0 0 0 0 0 1
                    , isInput      = False
                    }
                  ] ++ [
                    Variable
                    {
                      V.identifier = identifier'
                    , display      = Display
                                     {
                                       label      = label'
                                     , shortLabel = Just identifier'
                                     , color      = Nothing
                                     }
                    , domain       = Interval Nothing Nothing
                    , units        = Nothing -- FIXME Just $ Units 2 1 (-3) 0 0 0 0 0 1000
                    , isInput      = False
                    }
                  |
                    (label', identifier') <- meters site
                  ]
    primaryKey  = "time"
    timeKey     = Just "epoch"
  in
    Model{..}


siteModels :: Site -> [Model]
siteModels site@Site{..} =
  let
    prototype = siteModel site { siteMeters = [] }
  in
    [
      prototype
      {
        M.identifier = identifier'
      , uri          = Nothing -- FIXME
      , name         = label'
      , description  = Just "ADDITIONAL METADATA WILL BE ADDED SOON" -- FIXME
      , tags         = Just $ Tags [("units", "TO BE ADDED SOON")] -- FIXME
      , variables    = Variable
                       {
                         V.identifier = "measurement"
                       , display      = Display
                                        {
                                          label      = label'
                                        , shortLabel = Just identifier'
                                        , color      = Nothing
                                        }
                       , domain       = Interval Nothing Nothing
                       , units        = Nothing -- FIXME Just $ Units 2 1 (-3) 0 0 0 0 0 1000
                       , isInput      = False
                       }
                       : variables prototype
      }
    |
      (label', identifier') <- meters site
    ]
