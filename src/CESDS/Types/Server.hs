{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Server (
  ServerIdentifier
, APIVersion
, Server(..)
, Status(..)
) where


import CESDS.Types (Identifier, object')
import CESDS.Types.Model (ModelIdentifier)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Value(String), (.:), (.=), withObject, withText)
import Data.Text (Text)
import GHC.Generics (Generic)


type ServerIdentifier = Identifier


type APIVersion = Int


data Server =
  Server
  {
    identifier :: ServerIdentifier
  , version    :: APIVersion
  , models     :: [ModelIdentifier]
  , status     :: Status
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Server where
  parseJSON =
    withObject "SERVER" $ \o ->
      do
        identifier <- o .: "server_id"
        version    <- o .: "version"
        models     <- o .: "models"
        status     <- o .: "status"
        return Server{..}

instance ToJSON Server where
  toJSON Server{..} =
    object'
      [
        "server_id" .= identifier
      , "version"   .= version
      , "models"    .= models
      , "status"    .= status
      ]


data Status =
    Okay
  | Broken
  | OnFire
  | OtherStatus
    {
      message :: Text
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Status where
  parseJSON =
    withText "SERVER_STATUS" $ \s ->
      case s of
        "ok"      -> return Okay
        "broken"  -> return Broken
        "on_fire" -> return OnFire
        _         -> return $ OtherStatus s

instance ToJSON Status where
  toJSON Okay            = String "ok"
  toJSON Broken          = String "broken"
  toJSON OnFire          = String "on_fire"
  toJSON OtherStatus{..} = String message
