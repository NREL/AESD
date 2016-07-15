{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
, printForest
) where


import CESDS.Haystack (HaystackAccess, haystackNavTree)
import CESDS.Haystack.Cache (CacheManager(cache), makeCacheManager, refreshCacheManager, refreshExtractCacheManager)
import CESDS.Types (Identifier)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Function (on)
import Data.List (sortBy)
import Data.Yaml (decodeFile)
import NREL.Meters (metersRSF2)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as M


printForest :: HaystackAccess -> Maybe Identifier -> IO ()
printForest access identifier =
  putStrLn . LBS.unpack . encodePretty
    =<< haystackNavTree access identifier


main :: IO ()
main =
  do
    [configurationFile] <- getArgs
    Just access <- decodeFile configurationFile
    let
      sample = [head metersRSF2, metersRSF2 !! 6]
    cacheManager <- makeCacheManager access sample
    print $ length $ M.toList $ cache cacheManager
    cacheManager'   <- refreshCacheManager cacheManager  1468580000 (Just 1468585260)
    print $ length $ M.toList $ cache cacheManager'
    (cacheManager'', rows) <- refreshExtractCacheManager cacheManager' 1468570000 (Just 1468576000)
    print $ length $ M.toList $ cache cacheManager''
    sequence_
      [
        print (k, sortBy (compare `on` fst) $ H.toList v)
      |
        (k, v) <- M.toList $ cache cacheManager''
      ]
    sequence_
      [
        print $ sortBy (compare `on` fst) $ H.toList row
      |
        row <- rows
      ]
