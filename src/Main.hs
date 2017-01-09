{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Haystack.Cache.Memory (runCacheT, makeCache, refreshExtractCacheManager)
import CESDS.Records.Server (serverMain)
import CESDS.Records.Server.Manager (makeInMemoryManager)
import CESDS.Types.Model as Model (identifier)
import Control.Lens.Getter ((^.))
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
    [configuration, host, port, start, persistence] <- getArgs -- FIXME
    Just site <- decodeFile configuration
    let
      access = siteAccess site
    runResourceT
      $ do
        httpManager <- liftIO $ newManager tlsManagerSettings
        liftIO
          $ serverMain host (read port)
          =<< makeInMemoryManager (read persistence)
            (makeCache $ meters site)
            (
              return
                . (siteModels site, )
            )
            (
              \cache model ->
                 runToIO
                   . flip runCacheT cache
                   $ refreshExtractCacheManager httpManager access (pack $ model ^. Model.identifier) (Just $ read start) Nothing
            )
