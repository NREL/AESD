module Main (
  main
) where


import CESDS.Records.Server (buildModel, serverMain)
import CESDS.Types.Model (identifier, name, uri)
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Data.Hashable (hash)
import Data.List.Split (splitOn)
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))


main :: IO ()
main =
  do
    [host, port, directory] <- getArgs
    directory' <- makeAbsolute directory
    files <- getDirectoryContents directory'
    print =<< makeAbsolute directory
    serverMain host (read port)
      (
        sequence
        [
          (identifier .~ show (hash file))
            . (name .~ file)
            . (uri .~ "file://" ++ file)
            . fst
            . buildModel
            . fmap (splitOn "\t")
            . lines
            <$> readFile file
        |
          file <- (directory' </>) <$> files
        , ".tsv" `isSuffixOf` file
        ]
      )
      $ \m ->
        snd
          . buildModel
          . fmap (splitOn "\t")
          . lines
          <$> readFile (m ^. name)
