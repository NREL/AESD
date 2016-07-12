{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Server (
  ServerIdentifier
, APIVersion
, Server(..)
, validateServer
, Status(..)
) where


import CESDS.Types (Identifier, object')
import CESDS.Types.Model (ModelIdentifier)
import Control.Monad.Except (MonadError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Value(String), (.:), (.:?), (.!=), (.=), withObject)
import Data.Default (Default(..))
import Data.List.Util (noDuplicates)
import Data.String (IsString)
import Data.String.Util (maybeString)
import Data.Text (Text)
import GHC.Generics (Generic)


type ServerIdentifier = Identifier


type APIVersion = Int


data Server =
  Server
  {
    identifier :: ServerIdentifier
  , typ        :: Text
  , version    :: APIVersion
  , models     :: [ModelIdentifier]
  , status     :: Status
  }
    deriving (Eq, Generic, Read, Show)

instance Default Server where
  def =
    Server
    {
      identifier = "unidentified"
    , typ        = "record_server"
    , version    = 0
    , models     = []
    , status     = def
    }

instance FromJSON Server where
  parseJSON =
    withObject "SERVER" $ \o ->
      do
        identifier <- o .: "server_id"
        typ        <- o .: "server_type"
        version    <- o .: "version"
        models     <- o .: "models"
        status     <- o .: "status"
        return Server{..}

instance ToJSON Server where
  toJSON Server{..} =
    object'
      [
        "server_id"   .= identifier
      , "server_type" .= typ
      , "version"     .= version
      , "models"      .= models
      , "status"      .= status
      ]


validateServer :: (IsString e, MonadError e m) => Server -> m ()
validateServer Server{..} =
  do
    assert "empty server identifier"     $ identifier /= ""
    assert "illegal server type"         $ typ == "record_server"
    assert "illegal version number"      $ version == 0
    assert "duplicate model identifiers" $ noDuplicates models


data Status =
    Okay
    {
      message :: Text
    }
  | Failure
    {
      message :: Text
    }
    deriving (Eq, Generic, Read, Show)

instance Default Status where
  def = Okay ""

instance FromJSON Status where
  parseJSON =
    withObject "SERVER_STATUS" $ \o ->
      do
        state   <- o .:   "state"
        message <- o .:? "message" .!= ""
        case state of
          "ok"      -> return Okay{..}
          "failure" -> return Failure{..}
          _         -> fail $ "invalid server status: \"" ++ state ++ "\""

instance ToJSON Status where
  toJSON Okay{..}    = object' ["state" .= String "ok"     , "message" .= maybeString message]
  toJSON Failure{..} = object' ["state" .= String "failure", "message" .= maybeString message]
