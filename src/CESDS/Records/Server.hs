module CESDS.Records.Server (
  State
, serverMain
, buildModel
) where


import CESDS.Records (Cache, ModelCache, modelMeta)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier, varMeta)
import CESDS.Types.Record (RecordContent, filterRecords)
import CESDS.Types.Request as Request (onLoadModelsMeta, onLoadRecordsData, onRequest, requestIdentifier)
import CESDS.Types.Response as Response (chunkIdentifier, identifier, modelMetasResponse, nextChunkIdentifier, recordsResponse)
import CESDS.Types.Value (VarType(..), castVarType, consistentVarTypes)
import CESDS.Types.Variable as Variable (identifier, name, varType)
import Control.Arrow ((&&&))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (forM_)
import Data.Default (def)
import Data.List.Split (chunksOf)
import Data.Maybe (maybeToList)
import Network.WebSockets (Connection, acceptRequest, receiveData, runServer, sendBinaryData)

import qualified Data.Map.Strict as M (elems, fromList, lookup)


type State = (TVar Cache, Connection)


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
        . fmap ((^. Model.identifier) &&& (\m -> def {modelMeta = m}))
        =<< listModels
    runServer host port
      $ \pending ->
      do
        connection <- acceptRequest pending
        let
          loop =
            do
              request <- receiveData connection
              responses <- onRequest
                (
                  onLoadModelsMeta
                    $ fmap (Just . (: []) . modelMetasResponse)
                    . lookupModels cache False
                )
                (
                  onLoadRecordsData
                    $ \maybeModel count variables Nothing -> -- FIXME: Handle bookmarks.
                    do
                      models <- lookupModels cache True (Just maybeModel) :: IO [ModelMeta]
                      recs <-
                        (if null variables then id else filterRecords variables)
                          . concat
                          <$> mapM loadContent models
                      return
                        $ Just
                        [
                          recordsResponse recs''
                            & chunkIdentifier .~ Just i'
                            & nextChunkIdentifier .~ (if fromIntegral i' < n then Just (i' + 1) else Nothing)
                        |
                          let recs' = chunksOf (maybe maxBound fromIntegral count) recs
                        , let n = length recs'
                        , (i', recs'') <- zip [1..] recs'
                        ]
                )
                undefined
                undefined
                request
              forM_ responses
                . mapM_
                $ sendBinaryData connection
                . (Response.identifier .~ request ^. requestIdentifier)
              loop
        loop


lookupModelCaches :: TVar Cache -> Bool -> Maybe ModelIdentifier -> IO [ModelCache]
lookupModelCaches _     True   Nothing  = return []
lookupModelCaches cache False  Nothing  = M.elems  <$> atomically (readTVar cache)
lookupModelCaches cache _      (Just m) = maybeToList . M.lookup m <$> atomically (readTVar cache)


lookupModels :: TVar Cache -> Bool -> Maybe ModelIdentifier -> IO [ModelMeta]
lookupModels = (((fmap modelMeta <$>) .) .) . lookupModelCaches


--loadRecordsData :: TVar Cache -> Maybe ModelIdentifier -> [VariableIdentifier] -> Maybe BookmarkIdentifier -> [Response]
--loadRecordsData cache maybeMax maybeModel variables maybeBookmark =
--   undefined
--
--
--loadBookmarkMeta :: State -> ModelIdentifier -> Maybe BookmarkIdentifier -> [Response]
--loadBookmarkMeta = undefined


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
        & Model.varMeta .~ zipWith3 (\vid n t -> def & Variable.identifier .~ vid & name .~ n & varType .~ t) vids header types
    , zipWith (\rid vs -> (rid, zip vids vs)) rids
        $ zipWith castVarType types
        <$> values
    )
buildModel [] = (def, [])
