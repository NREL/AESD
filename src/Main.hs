{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

A server from Haystack.
-}


{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Haystack.Cache.Memory (runCacheT, makeCache, refreshExtractCacheManager)
import CESDS.Records.Server (serverMain)
import CESDS.Records.Server.Manager (makeInMemoryManager)
import CESDS.Types.Model as Model (identifier)
import Control.Monad.Except (liftIO)
import Control.Monad.Except.Util (runToIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (pack)
import Data.Yaml (decodeFile)
import NREL.Meters (Site(..), meters, siteModels)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import System.Environment (getArgs)


main :: IO ()
main =
  do
    [configuration, host, port, start, persistence, chunkSize] <- getArgs -- FIXME
    Just site <- decodeFile configuration
    let
      access = siteAccess site
    runResourceT
      $ do
        httpManager <- liftIO $ newManager tlsManagerSettings
        liftIO
          $ serverMain host (read port) (Just $ read chunkSize)
          =<< makeInMemoryManager (Just persistence)
            (makeCache $ meters site)
            (
              return
                . (siteModels site, )
            )
            (
              \cache model ->
                 runToIO
                   . flip runCacheT cache
                   $ refreshExtractCacheManager httpManager access (pack $ Model.identifier model) (Just $ read start) Nothing
            )
            (
              error "Sensor data only."
            )
