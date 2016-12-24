module Main (
  main
) where


import CESDS.Types.Model as M (identifier)
import CESDS.Types.Record (onRecordContent)
import CESDS.Types.Request (loadModelsMeta, loadRecordsData)
import CESDS.Types.Response (onResponse)
import Control.Lens.Getter ((^.))
import Control.Monad (void)
import Network.WebSockets (receiveData, runClient, sendBinaryData)



main :: IO ()
main =
  runClient "192.168.1.123" 50374 "/"
    $ \connection ->
    do
      sendBinaryData connection
        $ loadModelsMeta (Just 1) Nothing
      Just y <-
        onResponse
          (const . const $ return Nothing)
          (const $ return . Just . head)
          (const . const $ return Nothing)
          (const . const $ return Nothing)
        =<< receiveData connection
      sendBinaryData connection
        $ loadRecordsData (Just 2) (y ^. M.identifier) (Just 1) [] Nothing
      let
        loop :: Int -> IO ()
        loop n =
          do
            void
              $ onResponse
                (const $ (>> return Nothing) . print)
                (const $ (>> return Nothing) . print)
                (const $ (>> return Nothing) . print . head . onRecordContent Just (const Nothing) (const Nothing) Nothing)
                (const $ (>> return Nothing) . print)
              =<< receiveData connection
            loop $ n + 1
      loop 1
