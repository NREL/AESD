module Main (
  main
) where


import CESDS.Records.Server (buildModel, serverMain)
import CESDS.Types.Model (identifier, name, uri)
import Control.Lens.Setter ((.~))
import Data.List.Split (splitOn)


main :: IO ()
main =
  serverMain "127.0.0.1" 50374
    $ (: [])
    . (identifier .~ "test")
    . (name .~ "sampel")
    . (uri .~ "a uri")
    . fst
    . buildModel
    . fmap (splitOn "\t")
    . lines
    <$> getContents
