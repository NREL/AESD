{-|
Module      :  $Header$
Copyright   :  (c) 2017-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Server for MySQL.
-}


{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main (
-- * Entry point
  main
) where


import AESD.Records.Server.HDBC (hdbcMain)
import Control.Exception (bracket)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Yaml (decodeFileEither)
import Database.HDBC (disconnect)
import Database.HDBC.MySQL (MySQLConnectInfo(..), connectMySQL)
import GHC.Generics (Generic)
import System.Environment (getArgs)


-- | Configuration.
data Configuration =
  Configuration
  {
    host        :: String           -- ^ WebSocket host address.
  , port        :: Int              -- ^ WebSocket port number.
  , directory   :: FilePath         -- ^ Path to SQL queries.
  , persistence :: Maybe FilePath   -- ^ Path to persistnce journal.
  , chunkSize   :: Maybe Int        -- ^ The number of records per chunk.
  , database    :: MySQLConnectInfo -- ^ MySQL connection information.
  }
    deriving (Eq, Generic, Show)

instance FromJSON Configuration

instance ToJSON Configuration

deriving instance Eq MySQLConnectInfo

deriving instance Show MySQLConnectInfo

deriving instance Generic MySQLConnectInfo

instance FromJSON MySQLConnectInfo

instance ToJSON MySQLConnectInfo


-- | Run the server.
main :: IO ()
main =
  do
    [configuration] <- getArgs
    Right Configuration{..} <- decodeFileEither configuration
    bracket
      (connectMySQL database)
      disconnect
      $ hdbcMain True host port directory persistence chunkSize
