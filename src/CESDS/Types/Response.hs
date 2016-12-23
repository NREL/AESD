{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module CESDS.Types.Response (
  Response
, version
, identifier
, chunkIdentifier
, nextChunkIdentifier
, modelMetas
, records
, bookmarkMetas
, withResponse
) where


import CESDS.Types (OptionalInt32, VersionIdentifier, int32)
import CESDS.Types.Bookmark (BookmarkMeta, BookmarkMetas, bookmarks)
import CESDS.Types.Internal ()
import CESDS.Types.Model (ModelMeta, ModelMetas, models)
import CESDS.Types.Record (RecordContent, RecordData, recordData)
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
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
      (putField 3                       )
      (putField Nothing                 )
      (putField Nothing                 )
      (putField Nothing                 )
      (putField $ Just "invalid request")
      (putField Nothing                 )
      (putField Nothing                 )
      (putField Nothing                 )

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


modelMetas :: Lens' Response (Maybe [ModelMeta])
modelMetas =
  lens
    (fmap (^. models) . getField . modelMetas')
    (\s x -> s {modelMetas' = putField $ flip (models .~) def <$> x})


records :: Lens' Response (Maybe RecordData)
records = lens (getField . records') (\s x -> s {records' = putField x})


bookmarkMetas :: Lens' Response (Maybe [BookmarkMeta])
bookmarkMetas =
  lens
    (fmap (^. bookmarks) . getField . bookmarkMetas')
    (\s x -> s {bookmarkMetas' = putField $ flip (bookmarks .~) def <$> x})


withResponse :: Monad m => Response -> (String -> m a) -> ([ModelMeta] -> m a) -> ([RecordContent] -> m a) -> ([BookmarkMeta] -> m a) -> m (Maybe a)
withResponse x f g h i =
  maybe (return Nothing) (fmap Just)
     $  f              <$> x ^. responseError
    <|> g              <$> x ^. modelMetas
    <|> h . recordData <$> x ^. records
    <|> i              <$> x ^. bookmarkMetas
