{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Request (
  Request
, identifier
, onRequest
, LoadModelsMeta
, loadModelsMeta
, onLoadModelsMeta
, LoadRecordsData
, loadRecordsData
, onLoadRecordsData
, LoadBookmarkMeta
, loadBookmarkMeta
, onLoadBookmarkMeta
, SaveBookmarkMeta
, saveBookmarkMeta
, onSaveBookmarkMeta
) where


import CESDS.Types.Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Internal (OptionalInt32, int32)
import CESDS.Types.Model (ModelIdentifier)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
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


loadBookmarkMeta :: ModelIdentifier -> Maybe BookmarkIdentifier -> Request
loadBookmarkMeta m b = def {loadBookmarkMeta' = putField . Just $ LoadBookmarkMeta (putField m) (putField b)}


onLoadBookmarkMeta :: Monad m => (ModelIdentifier -> Maybe BookmarkIdentifier -> m a) -> LoadBookmarkMeta -> m a
onLoadBookmarkMeta f LoadBookmarkMeta{..} = f (getField loadBookmarkModelIdentifier) (getField loadBookmarkIdentifier)


data SaveBookmarkMeta =
  SaveBookmarkMeta
  {
    saveBookmarkModelIdentifier :: Required 1 (Value   ModelIdentifier)
  , saveBookmark                :: Required 2 (Message BookmarkMeta   )
  }
    deriving (Generic, Show)

instance Decode SaveBookmarkMeta

instance Encode SaveBookmarkMeta


saveBookmarkMeta :: ModelIdentifier -> BookmarkMeta -> Request
saveBookmarkMeta m b = def {saveBookmarkMeta' = putField . Just $ SaveBookmarkMeta (putField m) (putField b)}


onSaveBookmarkMeta :: Monad m => (ModelIdentifier -> BookmarkMeta -> m a) -> SaveBookmarkMeta -> m a
onSaveBookmarkMeta f SaveBookmarkMeta{..} = f (getField saveBookmarkModelIdentifier) (getField saveBookmark)


data LoadModelsMeta =
  LoadModelsMeta
  {
    modelIdentifier :: Optional 1 (Value ModelIdentifier)
  }
    deriving (Generic, Show)

instance Decode LoadModelsMeta

instance Encode LoadModelsMeta


loadModelsMeta :: Maybe ModelIdentifier -> Request
loadModelsMeta m = def {loadModelsMeta' = putField . Just . LoadModelsMeta $ putField m}


onLoadModelsMeta :: Monad m => (Maybe ModelIdentifier -> m a) -> LoadModelsMeta -> m a
onLoadModelsMeta f LoadModelsMeta{..} = f (getField modelIdentifier)


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


loadRecordsData :: ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Maybe BookmarkIdentifier -> Request
loadRecordsData m n v b = def {loadRecordsData' = putField . Just $ LoadRecordsData (putField m) (putField n) (putField v) (putField b)}


onLoadRecordsData :: Monad m => (ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Maybe BookmarkIdentifier -> m a) -> LoadRecordsData -> m a
onLoadRecordsData f LoadRecordsData{..} =
  f
    (getField modelIdentifier'   )
    (getField maxRecords         )
    (getField variableIdentifiers)
    (getField bookmarkIdentifier )


data Request =
  Request
  {
    requestVersion     :: Required 1 (Value   Word32          )
  , identifier' :: Optional 2 (Message OptionalInt32   )
  , loadModelsMeta'    :: Optional 3 (Message LoadModelsMeta  )
  , loadRecordsData'   :: Optional 4 (Message LoadRecordsData )
  , loadBookmarkMeta'  :: Optional 5 (Message LoadBookmarkMeta)
  , saveBookmarkMeta'  :: Optional 6 (Message SaveBookmarkMeta)
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


identifier :: Lens' Request (Maybe Int32)
identifier =
  lens
    (fmap (^. int32) . getField . identifier')
    (\s x -> s {identifier' = putField $ flip (int32 .~) def <$> x})


onRequest :: Monad m
            => (LoadModelsMeta   -> m a)
            -> (LoadRecordsData  -> m a)
            -> (LoadBookmarkMeta -> m a)
            -> (SaveBookmarkMeta -> m a)
            -> a
            -> Request
            -> m a
onRequest f g h i d Request{..} =
  fmap (fromMaybe d)
     . sequence
     $  f <$> getField loadModelsMeta'
    <|> g <$> getField loadRecordsData'
    <|> h <$> getField loadBookmarkMeta'
    <|> i <$> getField saveBookmarkMeta'
