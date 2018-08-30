{-|
Module      :  $Header$
Copyright   :  (c) 2016-18 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Example client.
-}


module Main (
-- * Entry point
  main
) where


import AESD.Types.Bookmark as Bookmark (identifier, name, makeSet)
import AESD.Types.Model as Model (identifier, varMeta)
import AESD.Records.Client (clientMain, fetchBookmarks, fetchModels, fetchRecords, storeBookmark)
import AESD.Types.Record (onRecordContent)
import AESD.Types.Variable as Variable (identifier, name)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Environment (getArgs)


-- | Action for running the client.
main :: IO ()
main =
  do
    [host, port] <- getArgs -- FIXME
    clientMain host (read port) "/"
      $ \state ->
        do
          m <- head . either error id <$> fetchModels state
          let
            i = Model.identifier m
          putStrLn ""
          putStrLn $ "Identifier for first model: " ++ i
          rs <- either error id <$> fetchRecords state i (Just 20)
          putStrLn ""
          putStrLn $ "Number of records: " ++ show (length rs)
          putStrLn ""
          putStrLn
            . intercalate "\t"
            $ ("#" :)
            [
              show (Variable.identifier v) ++ "=" ++ Variable.name v
            |
              v <- Model.varMeta m
            ]
          sequence_
            [
              putStrLn . intercalate "\t" $ show r : fmap snd vs 
            |
              (r, vs) <- onRecordContent show show show "?" rs
            ]
          putStrLn ""
          let
          b <-
            fmap (either error id)
              . storeBookmark state i
              $ makeSet
                Nothing
                "sample"
                (fst <$> take 2 rs)
          putStrLn $ "Stored bookmark \"" ++ Bookmark.name b ++ "\" identified by " ++ fromJust (Bookmark.identifier b) ++ "."
          putStrLn ""
          putStrLn "Boomarks:"
          bs <- either error id <$> fetchBookmarks state i Nothing
          sequence_
            [
              putStrLn $ "\t" ++ fromJust (Bookmark.identifier b') ++ "\t" ++ Bookmark.name b'
            |
              b' <- bs
            ]
          putStrLn ""
