{-|
Module      :  $Header$
Copyright   :  (c) 2017-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Server for ODBC.
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module Main (
-- * Entry point
  main
) where


import AESD.Records.Server.HDBC (hdbcMain)
import Control.Exception (bracket)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Yaml (decodeFileEither)
import Database.HDBC (disconnect)
import Database.HDBC.ODBC (connectODBC)
import GHC.Generics (Generic)
import System.Environment (getArgs)


-- Configuration.
data Configuration =
  Configuration
  {
    host        :: String         -- ^ WebSocket host address.
  , port        :: Int            -- ^ WebSocket port number.
  , directory   :: FilePath       -- ^ Path to SQL queries.
  , persistence :: Maybe FilePath -- ^ Path to persistnce journal.
  , chunkSize   :: Maybe Int      -- ^ The number of records per chunk.
  , database    :: String         -- ^ ODBC connection information.
  }
    deriving (Eq, Generic, Show)

instance FromJSON Configuration

instance ToJSON Configuration


-- | Run the server.
main :: IO ()
main =
  do
    [configuration] <- getArgs
    Right Configuration{..} <- decodeFileEither configuration
    bracket
      (connectODBC database)
      disconnect
      $ hdbcMain False host port directory persistence chunkSize
