module Main (
  main
) where


import CESDS.Types.Bookmark as Bookmark (identifier, name, setContent)
import CESDS.Types.Model as Model (identifier, varMeta)
import CESDS.Records.Client (clientMain, close, fetchBookmarks, fetchModels, fetchRecords, storeBookmark)
import CESDS.Types.Record (onRecordContent)
import CESDS.Types.Variable as Variable (identifier, name)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Default (def)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Environment (getArgs)


main :: IO ()
main =
  do
    [host, port] <- getArgs -- FIXME
    clientMain host (read port) "/"
      $ \state ->
        do
          m <- head . either error id <$> fetchModels state
          let
            i = m ^. Model.identifier
          putStrLn ""
          putStrLn $ "Identifier for first model: " ++ i
          rs <- either error id <$> fetchRecords state i
          putStrLn ""
          putStrLn $ "Number of records: " ++ show (length rs)
          putStrLn ""
          putStrLn
            . intercalate "\t"
            $ ("#" :)
            [
              show (v ^. Variable.identifier) ++ "=" ++ (v ^. Variable.name)
            |
              v <- m ^. Model.varMeta
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
              $ def
                & Bookmark.name .~ "sample"
                & Bookmark.setContent .~ Just (fst <$> take 2 rs)
          putStrLn $ "Stored bookmark \"" ++ (b ^. Bookmark.name) ++ "\" identified by " ++ fromJust (b ^. Bookmark.identifier) ++ "."
          putStrLn ""
          putStrLn "Boomarks:"
          bs <- either error id <$> fetchBookmarks state i Nothing
          sequence_
            [
              putStrLn $ "\t" ++ fromJust (b' ^. Bookmark.identifier) ++ "\t" ++ (b' ^. Bookmark.name)
            |
              b' <- bs
            ]
          putStrLn ""
          close state
