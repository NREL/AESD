{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module CESDS.Types.Response (
  Response
, version
, identifier
, chunkIdentifier
, nextChunkIdentifier
, errorResponse
, modelMetasResponse
, recordsResponse
, bookmarkMetasResponse
, onResponse
) where


import CESDS.Types (VersionIdentifier)
import CESDS.Types.Bookmark (BookmarkMeta, BookmarkMetas, bookmarks)
import CESDS.Types.Internal (OptionalInt32, int32)
import CESDS.Types.Model (ModelMeta, ModelMetas, models)
import CESDS.Types.Record (RecordContent, RecordData, recordData)
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~))
import Control.Monad (join)
import Data.Default (Default(..))
import Data.Int (Int32)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Required, Value, getField, putField)
import GHC.Generics (Generic)


data Response =
  Response
  {
    version'             :: Required 1 (Value   VersionIdentifier)
  , identifier'          :: Optional 2 (Message OptionalInt32    )
  , chunkIdentifier'     :: Optional 3 (Value   Int32            )
  , nextChunkIdentifier' :: Optional 4 (Value   Int32            )
  , responseError'       :: Optional 5 (Value   String           )
  , modelMetas'          :: Optional 6 (Message ModelMetas       )
  , records'             :: Optional 7 (Message RecordData       )
  , bookmarkMetas'       :: Optional 8 (Message BookmarkMetas    )
  }
    deriving (Generic, Show)

instance Default Response where
  def =
    Response
      (putField 3       )
      (putField Nothing )
      (putField Nothing )
      (putField Nothing )
      (putField Nothing )
      (putField Nothing )
      (putField Nothing )
      (putField Nothing )

instance Decode Response

instance Encode Response


version :: Lens' Response VersionIdentifier
version = lens (getField . version') (\s x -> s {version' = putField x})


identifier :: Lens' Response (Maybe Int32)
identifier =
  lens
    (fmap (^. int32) . getField . identifier')
    (\s x -> s {identifier' = putField $ flip (int32 .~) def <$> x})


chunkIdentifier :: Lens' Response (Maybe Int32)
chunkIdentifier = lens (getField . chunkIdentifier') (\s x -> s {chunkIdentifier' = putField x})


nextChunkIdentifier :: Lens' Response (Maybe Int32)
nextChunkIdentifier = lens (getField . nextChunkIdentifier') (\s x -> s {nextChunkIdentifier' = putField x})


responseError :: Lens' Response (Maybe String)
responseError = lens (getField . responseError') (\s x -> s {responseError' = putField x})


errorResponse :: String -> Response
errorResponse e = def & responseError .~ Just e


modelMetas :: Lens' Response (Maybe [ModelMeta])
modelMetas =
  lens
    (fmap (^. models) . getField . modelMetas')
    (\s x -> s {modelMetas' = putField $ flip (models .~) def <$> x})


modelMetasResponse :: [ModelMeta] -> Response
modelMetasResponse ms = def & modelMetas .~ Just ms


records :: Lens' Response (Maybe RecordData)
records = lens (getField . records') (\s x -> s {records' = putField x})


recordsResponse :: [RecordContent] -> Response
recordsResponse rs = def & records .~ Just (def & recordData .~ rs)


bookmarkMetas :: Lens' Response (Maybe [BookmarkMeta])
bookmarkMetas =
  lens
    (fmap (^. bookmarks) . getField . bookmarkMetas')
    (\s x -> s {bookmarkMetas' = putField $ flip (bookmarks .~) def <$> x})


bookmarkMetasResponse :: [BookmarkMeta] -> Response
bookmarkMetasResponse bs = def & bookmarkMetas .~ Just bs


onResponse :: Monad m
           => (Maybe Int32 -> String -> m (Maybe a))
           -> (Maybe Int32 -> [ModelMeta] -> m (Maybe a))
           -> (Maybe Int32 -> [RecordContent] -> m (Maybe a))
           -> (Maybe Int32 -> [BookmarkMeta] -> m (Maybe a))
           -> Response
           -> m (Maybe a)
onResponse f g h i x =
  let
    n = x ^. identifier
  in
    fmap join -- FIXME: Is there a simpler name for 'fmap join . sequence'?
       . sequence
       $  f n                   <$> x ^. responseError
      <|> g n                   <$> x ^. modelMetas   
      <|> h n . (^. recordData) <$> x ^. records      
      <|> i n                   <$> x ^. bookmarkMetas
