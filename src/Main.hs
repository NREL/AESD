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


main :: IO ()
main =
  do
    [configurationFile] <- getArgs
    Just access <- decodeFile configurationFile
    b <- navTree access navRoot
    putStrLn . LBS.unpack $ encodePretty b
