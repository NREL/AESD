module CESDS.Records.Server (
  State
, serverMain
, buildModel
) where


import CESDS.Records (Cache)
import CESDS.Types.Record (RecordContent, filterRecords)
import CESDS.Types.Request (onLoadModelsMeta, onLoadRecordsData, onRequest)
import CESDS.Types.Response (chunkIdentifier, modelMetasResponse, nextChunkIdentifier, recordsResponse)
import CESDS.Types.Value (VarType(..), castVarType, consistentVarTypes)
import CESDS.Types.Variable (identifier, name, varType)
import Control.Arrow ((&&&))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (void)
import Data.Default (def)
import Data.Int (Int32)
import Data.List.Split (chunksOf)
import Data.Maybe (maybeToList)
import Network.WebSockets (Connection, acceptRequest, receiveData, runServer, sendBinaryData)

import qualified CESDS.Types.Model as Model (ModelMeta, identifier, varMeta)
import qualified Data.Map.Strict as M ((!), elems, fromList, lookup)


type State = (Cache, Connection)


serverMain :: String
           -> Int
           -> IO [Model.ModelMeta]
           -> (Model.ModelMeta -> IO [RecordContent])
           -> IO ()
serverMain host port listModels loadContent=
  do
    cache <-
      newTVarIO
        . M.fromList
        . fmap ((^. Model.identifier) &&& id)
        =<< listModels
    runServer host port
      $ \pending ->
      do
        connection <- acceptRequest pending
        let
          loop =
            do
              request <- receiveData connection
              void $ onRequest
                (
                  \i ->
                    onLoadModelsMeta
                      (
                        \mi ->
                          do
                            c <- atomically $ readTVar cache
                            sendBinaryData connection
                              . modelMetasResponse i
                              $ maybe (M.elems c) ((maybeToList .) . flip M.lookup $ c) mi
                            return Nothing
                      )
                )
                (
                  \i ->
                    onLoadRecordsData
                      (
                        \mi mx vids Nothing ->
                          do
                            c <- atomically $ readTVar cache
                            let
                              m = c M.! mi
                            x <- filterRecords vids <$> loadContent m
                            sequence_
                             [
                               sendBinaryData connection
                                 $ recordsResponse i x'
                                 & chunkIdentifier .~ Just i'
                                 & nextChunkIdentifier .~ (if fromIntegral i' <= n then Just (i' + 1) else Nothing)
                             |
                               let xs = maybe (: []) chunksOf (fromIntegral <$> mx) $ x
                             , let n = length xs
                             , (i', x') <- zip [1..] xs
                             ]
                            return Nothing
                      )
                )
                ignore
                ignore
                request
              loop
        loop


ignore :: Monad m => Maybe Int32 -> a -> m (Maybe b)
ignore = const . const $ return Nothing


buildModel :: [[String]] -> (Model.ModelMeta, [RecordContent])
buildModel (header : content) =
  let
    values = fmap read <$> content
    types = foldl consistentVarTypes (const IntegerVar <$> header) values
    vids = [0..]
    rids = [0..]
  in
    (
      def
        & Model.varMeta .~ zipWith3 (\vid n t -> def & identifier .~ vid & name .~ n & varType .~ t) vids header types
    , zipWith (\rid vs -> (rid, zip vids vs)) rids
        $ zipWith castVarType types
        <$> values
    )
buildModel [] = (def, [])
