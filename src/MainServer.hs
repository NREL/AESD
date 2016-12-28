module Main (
  main
) where


import CESDS.Records.Server (buildModel)
import Data.List.Split (splitOn)


main :: IO ()
main =
  print
    . buildModel
    . fmap (splitOn "\t")
    . lines
    =<< getContents
