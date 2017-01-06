{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Haystack.Cache.Memory (runCacheT, makeCache, refreshExtractCacheManager)
import CESDS.Records.Server (serverMain)
import CESDS.Records.Server.Manager (makeInMemoryManager)
import CESDS.Types.Model as Model (identifier)
import Control.Lens.Getter ((^.))
import Control.Monad.Except (runExceptT, throwError)
import Data.Text (pack)
import Data.Yaml (decodeFile)
import NREL.Meters (Site(..), meters, siteModels)
import System.Environment (getArgs)


main :: IO ()
main =
  do
    [configuration, host, port, start, persistence] <- getArgs -- FIXME
    Just site <- decodeFile configuration
    let
      access = siteAccess site
    serverMain host (read port)
      =<< makeInMemoryManager (read persistence) -- FIXME: Add an HTTP session manager, maybe inside ResourceT; see <http://www.yesodweb.com/blog/2012/01/http-conduit>.
        (makeCache $ meters site)
        (
          return
            . (siteModels site, )
        )
        (
          \cache model ->
             (either (throwError . userError) return =<<)
               . runExceptT
               . flip runCacheT cache
               $ refreshExtractCacheManager access (pack $ model ^. Model.identifier) (Just $ read start) Nothing
        )
