{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Request (
  Request
, withRequest
, LoadModelsMeta
, loadModelsMeta
, withLoadModelsMeta
, LoadRecordsData
, loadRecordsData
, withLoadRecordsData
, LoadBookmarkMeta
, loadBookmarkMeta
, withLoadBookmarkMeta
, SaveBookmarkMeta
, saveBookmarkMeta
, withSaveBookmarkMeta
) where


import CESDS.Types (OptionalInt32, int32)
import CESDS.Types.Bookmark (BookmarkIdentifier)
import CESDS.Types.Internal ()
import CESDS.Types.Model (ModelIdentifier)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Control.Monad (join)
import Data.Default (Default(..))
import Data.Int (Int32)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Repeated, Required, Value, getField, putField)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)


data LoadBookmarkMeta =
  LoadBookmarkMeta
  {
    loadBookmarkModelIdentifier :: Required 1 (Value ModelIdentifier   )
  , loadBookmarkIdentifier      :: Optional 2 (Value BookmarkIdentifier)
  }
    deriving (Generic, Show)

instance Decode LoadBookmarkMeta

instance Encode LoadBookmarkMeta


loadBookmarkMeta :: Maybe Int32 -> ModelIdentifier -> Maybe BookmarkIdentifier -> Request
loadBookmarkMeta i m b = (request i) {loadBookmarkMeta' = putField . Just $ LoadBookmarkMeta (putField m) (putField b)}


withLoadBookmarkMeta :: Monad m => LoadBookmarkMeta -> (ModelIdentifier -> Maybe BookmarkIdentifier -> m a) -> m a
withLoadBookmarkMeta = flip uncurry . (getField . loadBookmarkModelIdentifier &&& getField . loadBookmarkIdentifier)


data SaveBookmarkMeta =
  SaveBookmarkMeta
  {
    saveBookmarkModelIdentifier :: Required 1 (Value ModelIdentifier   )
  , saveBookmarkIdentifier      :: Required 2 (Value BookmarkIdentifier)
  }
    deriving (Generic, Show)

instance Decode SaveBookmarkMeta

instance Encode SaveBookmarkMeta


saveBookmarkMeta :: Maybe Int32 -> ModelIdentifier -> BookmarkIdentifier -> Request
saveBookmarkMeta i m b = (request i) {saveBookmarkMeta' = putField . Just $ SaveBookmarkMeta (putField m) (putField b)}


withSaveBookmarkMeta :: Monad m => SaveBookmarkMeta -> (ModelIdentifier -> BookmarkIdentifier -> m a) -> m a
withSaveBookmarkMeta = flip uncurry . (getField . saveBookmarkModelIdentifier &&& getField . saveBookmarkIdentifier)


data LoadModelsMeta =
  LoadModelsMeta
  {
    modelIdentifier :: Optional 1 (Value ModelIdentifier)
  }
    deriving (Generic, Show)

instance Decode LoadModelsMeta

instance Encode LoadModelsMeta


loadModelsMeta :: Maybe Int32 -> Maybe ModelIdentifier -> Request
loadModelsMeta i m = (request i) {loadModelsMeta' = putField . Just . LoadModelsMeta $ putField m}


withLoadModelsMeta :: Monad m => LoadModelsMeta -> (Maybe ModelIdentifier -> m a) -> m a
withLoadModelsMeta = flip id . getField . modelIdentifier


data LoadRecordsData =
  LoadRecordsData
  {
    modelIdentifier'    :: Required 1 (Value ModelIdentifier   )
  , maxRecords          :: Optional 2 (Value Word64            )
  , variableIdentifiers :: Repeated 3 (Value VariableIdentifier)
  , bookmarkIdentifier  :: Optional 4 (Value BookmarkIdentifier)
  }
    deriving (Generic, Show)

instance Decode LoadRecordsData

instance Encode LoadRecordsData


loadRecordsData :: Maybe Int32 -> ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Maybe BookmarkIdentifier -> Request
loadRecordsData i m n v b = (request i) {loadRecordsData' = putField . Just $ LoadRecordsData (putField m) (putField n) (putField v) (putField b)}


withLoadRecordsData :: Monad m => LoadRecordsData -> (ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Maybe BookmarkIdentifier -> m a) -> m a
withLoadRecordsData LoadRecordsData{..} f =
  f
    (getField modelIdentifier'   )
    (getField maxRecords         )
    (getField variableIdentifiers)
    (getField bookmarkIdentifier )


data Request =
  Request
  {
    requestVersion    :: Required 1 (Value   Word32          )
  , requestIdentifier :: Optional 2 (Message OptionalInt32   )
  , loadModelsMeta'   :: Optional 3 (Message LoadModelsMeta  )
  , loadRecordsData'  :: Optional 4 (Message LoadRecordsData )
  , loadBookmarkMeta' :: Optional 5 (Message LoadBookmarkMeta)
  , saveBookmarkMeta' :: Optional 6 (Message SaveBookmarkMeta)
  }
    deriving (Generic, Show)

instance Default Request where
  def =
    Request
      (putField 3      )
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)

instance Decode Request

instance Encode Request


request :: Maybe Int32 -> Request
request i = def {requestIdentifier = putField $ flip (int32 .~ ) def <$> i}


withRequest :: Monad m
            => Request
            -> (Maybe Int32 -> LoadModelsMeta   -> m (Maybe a))
            -> (Maybe Int32 -> LoadRecordsData  -> m (Maybe a))
            -> (Maybe Int32 -> LoadBookmarkMeta -> m (Maybe a))
            -> (Maybe Int32 -> SaveBookmarkMeta -> m (Maybe a))
            -> m (Maybe a)
withRequest Request{..} f g h i =
  let
    t = (^. int32) <$> getField requestIdentifier
  in
    fmap join
       . sequence
       $  f t <$> getField loadModelsMeta'
      <|> g t <$> getField loadRecordsData'
      <|> h t <$> getField loadBookmarkMeta'
      <|> i t <$> getField saveBookmarkMeta'
