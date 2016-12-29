module CESDS.Records.Server (
  State
, serverMain
, buildModel
) where


import CESDS.Records (Cache)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Request (onLoadModelsMeta, onRequest)
import CESDS.Types.Value (VarType(..), castVarType, consistentVarTypes)
import CESDS.Types.Variable (identifier, name, varType)
import Control.Arrow ((&&&))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Default (def)
import Data.Int (Int32)
import Data.Maybe (maybeToList)
import Network.WebSockets (Connection, acceptRequest, receiveData, runServer, sendBinaryData)

import qualified CESDS.Types.Model as Model (ModelMeta, identifier, varMeta)
import qualified CESDS.Types.Response as Response (identifier, modelMetas)
import qualified Data.Map.Strict as M (elems, fromList, lookup)


type State = (Cache, Connection)


serverMain :: String
           -> Int
           -> IO [Model.ModelMeta]
           -> IO ()
serverMain host port listModels =
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
              onRequest
                (
                   \i r ->
                     onLoadModelsMeta
                       (
                         \mi ->
                           do
                             c <- atomically $ readTVar cache
                             let
                               x = maybe (M.elems c) ((maybeToList .) . flip M.lookup $ c) mi
                             sendBinaryData connection
                               $ def
                               & Response.identifier .~ i
                               & Response.modelMetas .~ Just x
                             return Nothing :: IO (Maybe ())
                       )
                       r
                )
                ignore
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
    vids = [1..]
    rids = [1..]
  in
    (
      def
        & Model.varMeta .~ zipWith3 (\vid n t -> def & identifier .~ vid & name .~ n & varType .~ t) vids header types
    , zipWith (\rid vs -> (rid, zip vids vs)) rids
        $ zipWith castVarType types
        <$> values
    )
