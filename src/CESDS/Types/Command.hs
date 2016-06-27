{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Command (
  Command(..)
, Result(..)
) where


import CESDS.Types (object')
import CESDS.Types.Model (ModelIdentifier)
import Control.Applicative ((<|>))
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import GHC.Generics (Generic)
import Data.Text (Text)


data Command =
    RestartServer
  | RestartModel
    {
      models :: [ModelIdentifier]
    }
  | ClearServer
  | ClearModel     
    {
      models :: [ModelIdentifier]
    }
  | StrategyRandom
    {
      models :: [ModelIdentifier]
    }
  | StrategyFIFO   
    {
      models :: [ModelIdentifier]
    }
  | StrategyFILO   
    {
      models :: [ModelIdentifier]
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Command where
  parseJSON =
    withObject "COMMAND" $ \o ->
      parseModelCommand o <|> parseServerCommand o
    where
      parseServerCommand o =
        do
          command <- o .: "command"
          case command of
            "restart" -> return RestartServer
            "clear"   -> return ClearServer
            _         -> fail $ "invalid COMMAND_OPTION \"" ++ command ++ "\""
      parseModelCommand o =
        do
          command <- o .: "command"
          models  <- o .: "param"
          case command of
           "restart"            -> return RestartModel{..}
           "clear"              -> return ClearModel{..}
           "model_strat_random" -> return StrategyRandom{..}
           "model_strat_fifo"   -> return StrategyFIFO{..}
           "model_strat_filo"   -> return StrategyFILO{..}
           _                    -> fail $ "invalid COMMAND_OPTION \"" ++ command ++ "\""

instance ToJSON Command where
  toJSON RestartServer      = object' ["command" .= ("restart"            :: String)                   ]
  toJSON ClearServer        = object' ["command" .= ("clear"              :: String)                   ]
  toJSON RestartModel{..}   = object' ["command" .= ("restart"            :: String), "param" .= models]
  toJSON ClearModel{..}     = object' ["command" .= ("clear"              :: String), "param" .= models]
  toJSON StrategyRandom{..} = object' ["command" .= ("model_strat_random" :: String), "param" .= models]
  toJSON StrategyFIFO{..}   = object' ["command" .= ("model_strat_fifo"   :: String), "param" .= models]
  toJSON StrategyFILO{..}   = object' ["command" .= ("model_strat_filo"   :: String), "param" .= models]
        

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
