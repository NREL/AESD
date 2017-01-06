{-# LANGUAGE TupleSections #-}


module Main (
  main
) where


import CESDS.Records.Server (serverMain)
import CESDS.Records.Server.File (buildModelContent, buildModelMeta)
import CESDS.Records.Server.Manager (makeInMemoryManager)
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
    [host, port, directory, persistence] <- getArgs -- FIXME
    directory' <- makeAbsolute directory
    files <- getDirectoryContents directory'
    serverMain host (read port)
      =<< makeInMemoryManager (read persistence) ()
      (
        const
          . fmap (, ())
          $ sequence
          [
            (identifier .~ show (hash file))
              . (name .~ file)
              . (uri .~ "file://" ++ file)
              . buildModelMeta
              . fmap (splitOn "\t")
              . lines
              <$> readFile file
          |
            file <- (directory' </>) <$> files
          , ".tsv" `isSuffixOf` file
          ]
      )
      ( \_ m ->
        (, ())
          . buildModelContent
          . fmap (splitOn "\t")
          . lines
          <$> readFile (m ^. name)
      )
