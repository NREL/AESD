{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
, printForest
, navRoot
, navRSF2
) where


import CESDS.Haystack
import CESDS.Types (Identifier)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Yaml (decodeFile)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)


navRoot :: Maybe Identifier
navRoot = Nothing

navRSF2 :: Maybe Identifier
navRSF2 = Just "`equip:/1edb6bcb-7a9026e4`"

idRSF2MainPower :: Identifier
idRSF2MainPower = "@1edb6d30-f64869a4"


printForest :: HaystackAccess -> Maybe Identifier -> IO ()
printForest access identifier =
  putStrLn . LBS.unpack . encodePretty
    =<< haystackNavTree access identifier


main :: IO ()
main =
  do
    [configurationFile] <- getArgs
    Just access <- decodeFile configurationFile
    b <- haystackHisRead access idRSF2MainPower (AfterTime "2016-07-14T17:09:00-06:00 Denver") -- Today
    print b
