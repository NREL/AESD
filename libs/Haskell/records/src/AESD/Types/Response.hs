{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for responses from a server.

Typically, one might construct a response as follows:

>>> import AESD.Types.Bookmark (makeSet)
>>> import AESD.Types.Response (bookmarkMetasResponse, chunkIdentifier, identifier)
>>> import Control.Lens ((&), (.~), (^.))
>>>
>>> let b = makeSet (Just "42") "sample set bookmark" [1066, 1812, 1939]
>>> let r = bookmarkMetasResponse [b] & identifier .~ 300 & chunkIdentifier .~ 3
-}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module AESD.Types.Response (
-- * Responses
  Response
, version
, identifier
, chunkIdentifier
, nextChunkIdentifier
-- * Construction
, errorResponse
, modelMetasResponse
, recordsResponse
, bookmarkMetasResponse
-- * Handling
, onResponse
) where


import AESD.Types (VersionIdentifier, currentVersion)
import AESD.Types.Bookmark (BookmarkMeta, BookmarkMetas, bookmarks, makeBookmarks)
import AESD.Types.Internal (OptionalUInt32, makeUint32, uint32)
import AESD.Types.Model (ModelMeta, ModelMetas, makeModels, models)
import AESD.Types.Record (RecordContent, RecordData, makeRecordData, recordData)
import AESD.Types.Request (RequestIdentifier)
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Required, Value, getField, putField)
import GHC.Generics (Generic)


-- | A response from a server.
data Response =
  Response
  {
    version'             :: Required 1 (Value   VersionIdentifier)
  , identifier'          :: Optional 2 (Message OptionalUInt32   )
  , chunkIdentifier'     :: Optional 3 (Value   Int32            )
  , nextChunkIdentifier' :: Optional 4 (Value   Int32            )
  , responseError'       :: Optional 5 (Value   String           )
  , modelMetas'          :: Optional 6 (Message ModelMetas       )
  , records'             :: Optional 7 (Message RecordData       )
  , bookmarkMetas'       :: Optional 8 (Message BookmarkMetas    )
  }
    deriving (Generic, Show)


instance Decode Response

instance Encode Response


-- | Empty response.
{-# INLINE def #-}
def :: Response
def =
  Response
  {
    version'             = putField currentVersion
  , identifier'          = mempty
  , chunkIdentifier'     = mempty
  , nextChunkIdentifier' = mempty
  , responseError'       = mempty
  , modelMetas'          = mempty
  , records'             = mempty
  , bookmarkMetas'       = mempty
  }


-- | Lens for the version of a request.
version :: Lens' Response VersionIdentifier
version = lens (getField . version') (\s x -> s {version' = putField x})


-- | Lens for the unique identifier of a request.
identifier :: Lens' Response (Maybe RequestIdentifier)
identifier =
  lens
    (fmap uint32 . getField . identifier')
    (\s x -> s {identifier' = putField $ makeUint32 <$> x})


-- | Lens for the chunk identifier of a request.
chunkIdentifier :: Lens' Response (Maybe Int32)
chunkIdentifier = lens (getField . chunkIdentifier') (\s x -> s {chunkIdentifier' = putField x})


-- | Lens for the next chunk identifier of a request.
nextChunkIdentifier :: Lens' Response (Maybe Int32)
nextChunkIdentifier = lens (getField . nextChunkIdentifier') (\s x -> s {nextChunkIdentifier' = putField x})


-- | Construct an error response.
errorResponse :: String -> Response
errorResponse e = def {responseError' = putField $ Just e}


-- | Construct a response of model metadata.
modelMetasResponse :: [ModelMeta] -> Response
modelMetasResponse ms = def {modelMetas' = putField . Just $ makeModels ms}


-- | Construct a response of records data.
recordsResponse :: [RecordContent] -> Response
recordsResponse rs = def {records' = putField . Just $ makeRecordData rs}


-- | Construct a response of bookmark metadata.
bookmarkMetasResponse :: [BookmarkMeta] -> Response
bookmarkMetasResponse bs = def {bookmarkMetas' = putField . Just $ makeBookmarks bs}


-- | Handle a resonse.
onResponse :: Monad m
           => (Maybe RequestIdentifier -> String -> m a)          -- ^ Handle an error.
           -> (Maybe RequestIdentifier -> [ModelMeta] -> m a)     -- ^ Handle model metadata.
           -> (Maybe RequestIdentifier -> [RecordContent] -> m a) -- ^ Handle record data.
           -> (Maybe RequestIdentifier -> [BookmarkMeta] -> m a)  -- ^ Handle bookmark metadata.
           -> a                                        -- ^ The default result.
           -> Response                                 -- ^ The response.
           -> m a                                      -- ^ The action for handling a response.
onResponse f g h i d x =
  let
    n = x ^. identifier
  in
    fmap (fromMaybe d)
       . sequence
       $  f n              <$> getField (responseError' x)
      <|> g n . models     <$> getField (modelMetas'    x)
      <|> h n . recordData <$> getField (records'       x)
      <|> i n . bookmarks  <$> getField (bookmarkMetas' x)
