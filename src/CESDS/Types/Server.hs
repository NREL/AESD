{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Server (
  ServerIdentifier
, APIVersion
, Server(..)
, ServerStatus(..)
) where


import CESDS.Types (Identifier)
import CESDS.Types.Model (ModelIdentifier)
import Data.Aeson (FromJSON, ToJSON)
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
  , status     :: ServerStatus
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


data ServerStatus =
    Okay
  | Broken
  | OnFire
  | OtherStatus
    {
      message :: Text
    }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
