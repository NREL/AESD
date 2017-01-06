module Main (
  main
) where


import CESDS.Types.Model as Model (identifier, varMeta)
import CESDS.Records.Client (clientMain, close, fetchModels, fetchRecords)
import CESDS.Types.Record (onRecordContent)
import CESDS.Types.Variable as Variable (identifier, name)
import Control.Lens.Getter ((^.))
import Data.List (intercalate)
import System.Environment (getArgs)


main :: IO ()
main =
  do
    [host, port] <- getArgs -- FIXME
    clientMain host (read port) "/"
      $ \state ->
        do
          m <- head <$> fetchModels state
          let
            i = m ^. Model.identifier
          putStrLn ""
          putStrLn $ "Identifier for first model: " ++ i
          rs <- fetchRecords state i
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
          close state
