{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
, navRoot
, navRSF2
) where


import CESDS.Haystack
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Yaml (decodeFile)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)


navRoot :: Maybe String
navRoot = Nothing

navRSF2 :: Maybe String
navRSF2 = Just "`equip:/1edb6bcb-7a9026e4`"

idRSF2MainPower :: String
idRSF2MainPower = "@1edb6d30-f64869a4"


main :: IO ()
main =
  do
    [configurationFile] <- getArgs
    Just access <- decodeFile configurationFile
    b <- haystackRead access idRSF2MainPower
--  b <- haystackNavTree access navRoot
    putStrLn . LBS.unpack $ encodePretty b
