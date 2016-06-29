{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Command (
  Command(..)
, Result(..)
) where


import CESDS.Types (object')
import Control.Applicative ((<|>))
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Value, (.:), (.:?), (.=), withObject)
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
  | StrategyRandom
    {
      parameters :: [Value]
    }
  | StrategyFIFO   
    {
      parameters :: [Value]
    }
  | StrategyFILO   
    {
      parameters :: [Value]
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Command where
  parseJSON =
    withObject "COMMAND" $ \o ->
      do
        command     <- o .: "command"
        parameters  <- o .: "param"
        case command of
         "restart"            -> return Restart{..}
         "clear"              -> return Clear{..}
         "model_strat_random" -> return StrategyRandom{..}
         "model_strat_fifo"   -> return StrategyFIFO{..}
         "model_strat_filo"   -> return StrategyFILO{..}
         _                    -> fail $ "invalid COMMAND_OPTION \"" ++ command ++ "\""

instance ToJSON Command where
  toJSON Restart{..}        = object' ["command" .= ("restart"            :: String), "param" .= parameters]
  toJSON Clear{..}          = object' ["command" .= ("clear"              :: String), "param" .= parameters]
  toJSON StrategyRandom{..} = object' ["command" .= ("model_strat_random" :: String), "param" .= parameters]
  toJSON StrategyFIFO{..}   = object' ["command" .= ("model_strat_fifo"   :: String), "param" .= parameters]
  toJSON StrategyFILO{..}   = object' ["command" .= ("model_strat_filo"   :: String), "param" .= parameters]
        

data Result =
    Success
  | Error
    {
      code    :: Text
    , message :: Maybe Text
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Result where
  parseJSON = withObject "COMMAND_RESULT" $ \o ->
    parseError o <|> return Success
    where
      parseError o =
        do
          code    <- o .:  "result"
          message <- o .:? "additional"
          return Error{..}

instance ToJSON Result where
  toJSON Success   = object' [                                          ]
  toJSON Error{..} = object' ["result" .= code , "additional" .= message]
