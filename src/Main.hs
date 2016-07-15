{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
, printForest
) where


import CESDS.Haystack
import CESDS.Haystack.Cache
import CESDS.Types (Identifier)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Function (on)
import Data.List (sortBy)
import Data.Yaml (decodeFile)
import NREL.Meters (nrelRSF2)
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
      sample = map snd [head nrelRSF2, nrelRSF2 !! 6]
      start = "2016-07-14T19:09:00-06:00 Denver"
    histories <- mapM (flip (haystackHisRead access) $ AfterTime start) sample
    let
      cache = foldl addHistory newCache $ zip sample histories
    sequence_
      [
        print (k, sortBy (compare `on` fst) $ H.toList v)
      |
        (k, v) <- M.toList cache
      ]
