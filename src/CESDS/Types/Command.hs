{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Command (
  Command(..)
, Result(..)
) where


import CESDS.Types.Model (ModelIdentifier)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)


data Command =
    RestartServer
  | RestartModel
    {
      model :: ModelIdentifier
    }
  | ClearServer
  | ClearModel     
    {
      model :: ModelIdentifier
    }
  | StrategyRandom
    {
      model :: ModelIdentifier
    }
  | StrategyFIFO   
    {
      model :: ModelIdentifier
    }
  | StrategyFILO   
    {
      model :: ModelIdentifier
    }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


data Result =
    Success
  | Error
    {
      message :: String
    }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
