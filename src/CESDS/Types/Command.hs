{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Command (
  Command(..)
, isServerCommand
, Result(..)
) where


import CESDS.Types (object')
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Value, (.:), (.:?), (.!=), (.=), withObject)
import GHC.Generics (Generic)
import Data.Text (Text)


data Command =
    Restart
    {
      parameters :: [Value]
    }
  | Clear
    {
      parameters :: [Value]
    }
  | SetStrategy
    {
      parameters :: [Value]
    }
  | GetStrategy
    {
      parameters :: [Value]
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Command where
  parseJSON =
    withObject "COMMAND" $ \o ->
      do
        command     <- o .:  "command"
        parameters  <- o .:? "param"   .!= []
        case command of
         "restart"            -> return Restart{..}
         "clear"              -> return Clear{..}
         "set_model_strategy" -> return SetStrategy{..}
         "get_model_strategy" -> return GetStrategy{..}
         _                    -> fail $ "invalid COMMAND_OPTION \"" ++ command ++ "\""

instance ToJSON Command where
  toJSON Restart{..}      = object' ["command" .= ("restart"            :: String), "param" .= parameters]
  toJSON Clear{..}        = object' ["command" .= ("clear"              :: String), "param" .= parameters]
  toJSON SetStrategy{..}  = object' ["command" .= ("set_model_strategy" :: String), "param" .= parameters]
  toJSON GetStrategy{..}  = object' ["command" .= ("get_model_strategy" :: String), "param" .= parameters]


isServerCommand :: Command -> Bool
isServerCommand Restart{} = True
isServerCommand Clear{}   = True
isServerCommand _         = False
        

data Result =
    Result
    {
      message :: Maybe Text
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Result where
  parseJSON = withObject "COMMAND_RESULT" $ \o ->
    do
      message <- o .:? "result"
      return Result{..}

instance ToJSON Result where
  toJSON Result{..} = object' ["result" .= message]
