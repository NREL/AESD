module Main (
  main
) where


import CESDS.Types.Record (withRecordContent)
import CESDS.Types.Request (loadModelsMeta, loadRecordsData)
import CESDS.Types.Response (withResponse)
import Control.Lens.Getter ((^.))
import Control.Monad (void)
import Network.WebSockets (receiveData, runClient, sendBinaryData)

import qualified CESDS.Types.Model as M


main :: IO ()
main =
  runClient "192.168.1.123" 50374 "/"
    $ \connection ->
    do
      sendBinaryData connection $ loadModelsMeta (Just 1) Nothing
      x <- receiveData connection
      Just y <- withResponse x
        (const . const $ return Nothing)
        (const $ return . Just . head)
        (const . const $ return Nothing)
        (const . const $ return Nothing)
      sendBinaryData connection $ loadRecordsData (Just 2) (y ^. M.identifier) (Just 1) [] Nothing
      let
        loop :: Int -> IO ()
        loop n =
          do
            z <- receiveData connection
            void $ withResponse z
              (const $ (>> return Nothing) . print)
              (const $ (>> return Nothing) . print)
              (const $ (>> return Nothing) . print . head . (\w -> withRecordContent w Just (const Nothing) (const Nothing) Nothing))
              (const $ (>> return Nothing) . print)
            loop $ n + 1
      loop 1
