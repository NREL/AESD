{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
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


import CESDS.Types.Bookmark as Bookmark (identifier, name, makeSet)
import CESDS.Types.Model as Model (identifier, varMeta)
import CESDS.Records.Client (clientMain, close, fetchBookmarks, fetchModels, fetchRecords, storeBookmark)
import CESDS.Types.Record (onRecordContent)
import CESDS.Types.Variable as Variable (identifier, name)
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
          close state
