{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Request (
  Request
, identifier
, subscribe
, onRequest
, LoadModelsMeta
, loadModelsMeta
, onLoadModelsMeta
, LoadRecordsData
, loadRecordsData
, loadBookmarkedRecordsData
, loadFilteredRecordsData
, onLoadRecordsData
, LoadBookmarkMeta
, loadBookmarkMeta
, onLoadBookmarkMeta
, SaveBookmarkMeta
, saveBookmarkMeta
, onSaveBookmarkMeta
, Work
, work
, onWork
, Cancel
, cancel
, onCancel
) where


import CESDS.Types.Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Filter (Filter, FilterExpression, fromExpression, toExpression)
import CESDS.Types.Internal (OptionalUInt32, OptionalString, uint32, string)
import CESDS.Types.Model (ModelIdentifier)
import CESDS.Types.Record (VarValue)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Repeated, Required, Value, getField, putField)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)


data LoadBookmarkMeta =
  LoadBookmarkMeta
  {
    loadBookmarkModelIdentifier :: Required 1 (Value ModelIdentifier )
  , loadBookmarkIdentifier      :: Optional 2 (Message OptionalString)
  }
    deriving (Generic, Show)

instance Decode LoadBookmarkMeta

instance Encode LoadBookmarkMeta


loadBookmarkMeta :: ModelIdentifier -> Maybe BookmarkIdentifier -> Request
loadBookmarkMeta m b = def {loadBookmarkMeta' = putField . Just $ LoadBookmarkMeta (putField m) (putField $ flip (string .~) def <$> b)}


onLoadBookmarkMeta :: Monad m => (ModelIdentifier -> Maybe BookmarkIdentifier -> m a) -> LoadBookmarkMeta -> m a
onLoadBookmarkMeta f LoadBookmarkMeta{..} = f (getField loadBookmarkModelIdentifier) ((^. string) <$> getField loadBookmarkIdentifier)


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
    modelIdentifier :: Optional 1 (Message OptionalString)
  }
    deriving (Generic, Show)

instance Decode LoadModelsMeta

instance Encode LoadModelsMeta


loadModelsMeta :: Maybe ModelIdentifier -> Request
loadModelsMeta m = def {loadModelsMeta' = putField . Just . LoadModelsMeta . putField $ flip (string .~) def <$> m}


onLoadModelsMeta :: Monad m => (Maybe ModelIdentifier -> m a) -> LoadModelsMeta -> m a
onLoadModelsMeta f LoadModelsMeta{..} = f ((^. string) <$> getField modelIdentifier)


data LoadRecordsData =
  LoadRecordsData
  {
    modelIdentifier'    :: Required 1 (Value   ModelIdentifier   )
  , maxRecords          :: Optional 2 (Value   Word64            )
  , variableIdentifiers :: Repeated 3 (Value   VariableIdentifier)
  , bookmarkIdentifier  :: Optional 4 (Value   BookmarkIdentifier)
  , filterExpression    :: Optional 5 (Message FilterExpression  )
  }
    deriving (Generic, Show)

instance Decode LoadRecordsData

instance Encode LoadRecordsData


loadRecordsData :: ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Request
loadRecordsData m n v = def {loadRecordsData' = putField . Just $ LoadRecordsData (putField m) (putField n) (putField v) (putField Nothing) (putField Nothing)}


loadBookmarkedRecordsData :: ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> BookmarkIdentifier -> Request
loadBookmarkedRecordsData m n v b = def {loadRecordsData' = putField . Just $ LoadRecordsData (putField m) (putField n) (putField v) (putField $ Just b) (putField Nothing)}


loadFilteredRecordsData :: ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Filter -> Request
loadFilteredRecordsData m n v f = def {loadRecordsData' = putField . Just $ LoadRecordsData (putField m) (putField n) (putField v) (putField Nothing) (putField . Just $ toExpression f)}


onLoadRecordsData :: Monad m => (ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Maybe (Either BookmarkIdentifier Filter) -> m a) -> LoadRecordsData -> m a
onLoadRecordsData f LoadRecordsData{..} =
  f
    (getField modelIdentifier'   )
    (getField maxRecords         )
    (getField variableIdentifiers)
    $ Left <$> getField bookmarkIdentifier <|> Right . fromExpression <$> getField filterExpression


data Work =
  Work
  {
    workModel'  :: Required 1 (Value   String  )
  , workInputs' :: Repeated 2 (Message VarValue)
  }
    deriving (Generic, Show)

instance Default Work where
  def = Work (putField def) (putField def)

instance Decode Work

instance Encode Work


work :: ModelIdentifier -> [VarValue] -> Request
work m is = def {work' = putField $ Just Work {workModel' = putField m, workInputs' = putField is}}


onWork :: Monad m => (ModelIdentifier -> [VarValue] -> m a) -> Work -> m a
onWork f Work{..} = f (getField workModel') (getField workInputs')


data Cancel =
  Cancel
  {
    cancelIdentifier' :: Required 1 (Message OptionalUInt32)
  }
    deriving (Generic, Show)

instance Default Cancel where
  def = Cancel (putField def)

instance Decode Cancel

instance Encode Cancel


cancel :: Word32 -> Request
cancel i = def {cancel' = putField . Just $ Cancel {cancelIdentifier' = putField $ def & uint32 .~ i}}


onCancel :: Monad m => (Word32 -> m a) -> Cancel -> m a
onCancel f Cancel{..} = f $ getField cancelIdentifier' ^. uint32

 
data Request =
  Request
  {
    requestVersion     :: Required 1 (Value   Word32          )
  , identifier'        :: Optional 2 (Message OptionalUInt32  )
  , subscribe'         :: Optional 3 (Value   Bool            )
  , loadModelsMeta'    :: Optional 4 (Message LoadModelsMeta  )
  , loadRecordsData'   :: Optional 5 (Message LoadRecordsData )
  , loadBookmarkMeta'  :: Optional 6 (Message LoadBookmarkMeta)
  , saveBookmarkMeta'  :: Optional 7 (Message SaveBookmarkMeta)
  , cancel'            :: Optional 8 (Message Cancel          )
  , work'              :: Optional 9 (Message Work            )
  }
    deriving (Generic, Show)

instance Default Request where
  def =
    Request
      (putField 4      )
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)
      (putField Nothing)

instance Decode Request

instance Encode Request


identifier :: Lens' Request (Maybe Word32)
identifier =
  lens
    (fmap (^. uint32) . getField . identifier')
    (\s x -> s {identifier' = putField $ flip (uint32 .~) def <$> x})


subscribe :: Lens' Request Bool
subscribe =
  lens
    (fromMaybe False . getField . subscribe')
    (\s x -> s {subscribe' = putField $ Just x})


onRequest :: Monad m
            => (LoadModelsMeta   -> m a)
            -> (LoadRecordsData  -> m a)
            -> (LoadBookmarkMeta -> m a)
            -> (SaveBookmarkMeta -> m a)
            -> (Cancel           -> m a)
            -> (Work             -> m a)
            -> a
            -> Request
            -> m a
onRequest f g h i j k d Request{..} =
  fmap (fromMaybe d)
     . sequence
     $  f <$> getField loadModelsMeta'
    <|> g <$> getField loadRecordsData'
    <|> h <$> getField loadBookmarkMeta'
    <|> i <$> getField saveBookmarkMeta'
    <|> j <$> getField cancel'
    <|> k <$> getField work'
