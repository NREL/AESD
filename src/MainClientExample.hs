module Main (
  main
) where


import CESDS.Records.Client (clientMain, close, fetchModels, fetchRecords)
import CESDS.Types.Model (identifier)
import Control.Lens.Getter ((^.))


main :: IO ()
main =
--clientMain "192.168.1.123" 50374 "/"
  clientMain "127.0.0.1"     50374 "/"
    $ \state ->
      do
        i <- (^. identifier) . head <$>  fetchModels state
        putStrLn $ "Identifier for first model: " ++ show i
--      rs <- fetchRecords state i
--      putStrLn $ "Number of records: " ++ show (length rs)
        close state
