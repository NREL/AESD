{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module Main (
  main
) where


import CESDS.Records.Server.HDBC (hdbcMain)
import Control.Exception (bracket)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Yaml (decodeFile)
import Database.HDBC (disconnect)
import Database.HDBC.ODBC (connectODBC)
import GHC.Generics (Generic)
import System.Environment (getArgs)


data Configuration =
  Configuration
  {
    host        :: String
  , port        :: Int
  , directory   :: FilePath
  , persistence :: Maybe FilePath
  , database    :: FilePath
  }
    deriving (Eq, Generic, Show)

instance FromJSON Configuration

instance ToJSON Configuration


main :: IO ()
main =
  do
    [configuration] <- getArgs
    Just Configuration{..} <- decodeFile configuration
    bracket
      (connectODBC database)
      disconnect
      $ hdbcMain host port directory persistence
