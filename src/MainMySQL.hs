{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main (
  main
) where


import CESDS.Records.Server.HDBC (hdbcMain)
import Control.Exception (bracket)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Yaml (decodeFile)
import Database.HDBC (disconnect)
import Database.HDBC.MySQL (MySQLConnectInfo(..), connectMySQL)
import GHC.Generics (Generic)
import System.Environment (getArgs)


data Configuration =
  Configuration
  {
    host        :: String
  , port        :: Int
  , directory   :: FilePath
  , persistence :: Maybe FilePath
  , database    :: MySQLConnectInfo
  }
    deriving (Eq, Generic, Show)

instance FromJSON Configuration

instance ToJSON Configuration


deriving instance Eq MySQLConnectInfo

deriving instance Show MySQLConnectInfo

deriving instance Generic MySQLConnectInfo

instance FromJSON MySQLConnectInfo

instance ToJSON MySQLConnectInfo


main :: IO ()
main =
  do
    [configuration] <- getArgs
    Just Configuration{..} <- decodeFile configuration
    bracket
      (connectMySQL database)
      disconnect
      $ hdbcMain host port directory persistence
