{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Server for tabular files.
-}


{-# LANGUAGE TupleSections #-}


module Main (
-- * Entry point
  main
) where


import CESDS.Records.Server (serverMain)
import CESDS.Records.Server.File (buildModelContent, buildVarMetas)
import CESDS.Records.Server.Manager (makeInMemoryManager)
import CESDS.Types.Model (makeModelMeta, name)
import Data.List.Split (splitOn)
import Data.List (isSuffixOf)
import Data.UUID (toString)
import Data.UUID.V5 (generateNamed, namespaceURL)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))


-- | Action for running the server.
main :: IO ()
main =
  do
    [host, port, directory, persistence, chunkSize] <- getArgs -- FIXME
    directory' <- makeAbsolute directory
    files <- getDirectoryContents directory'
    serverMain host (read port) (Just $ read chunkSize)
      =<< makeInMemoryManager (Just persistence) ()
      (
        -- Load the model metadata.
        const
          . fmap (, ())
          $ sequence
          [
            flip
              (
                makeModelMeta
                  (toString $ generateNamed namespaceURL $ toEnum . fromEnum <$> "file://" ++ file)
                  file
                  ("file://" ++ file)
                  . buildVarMetas
                  . fmap (splitOn "\t")
                  . lines
              )
              []
              <$> readFile file
          |
            file <- (directory' </>) <$> files
          , ".tsv" `isSuffixOf` file
          ]
      )
      ( -- Load the data records.
        \_ m ->
          (, ())
            . buildModelContent
            . fmap (splitOn "\t")
            . lines
            <$> readFile (name m)
      )
      (
        error "Static data only."
      )
