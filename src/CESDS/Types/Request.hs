{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for making requests to servers.  The following requests are supported: 'LoadModelsMeta', 'LoadRecordsData', 'LoadBookmarkMeta', 'SaveBookmarkMeta', 'Work', and 'Cancel'.
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Request (
-- * Requests
  RequestIdentifier
, Request
, identifier
, subscribe
, onRequest
-- * Loading models
, LoadModelsMeta
, loadModelsMeta
, onLoadModelsMeta
-- * Loading recordsdata
, LoadRecordsData
, loadRecordsData
, loadBookmarkedRecordsData
, loadFilteredRecordsData
, onLoadRecordsData
, onLoadRecordsData'
-- * Loading bookmarks
, LoadBookmarkMeta
, loadBookmarkMeta
, onLoadBookmarkMeta
-- * Saving bookmarks
, SaveBookmarkMeta
, saveBookmarkMeta
, onSaveBookmarkMeta
-- * Creating work
, Work
, work
, onWork
-- * Canceling requests
, Cancel
, cancel
, onCancel
) where


import CESDS.Types (VersionIdentifier, currentVersion)
import CESDS.Types.Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Filter (Filter, FilterExpression, fromExpression, toExpression)
import CESDS.Types.Internal (OptionalUInt32, OptionalString, makeString, makeUint32, string, uint32)
import CESDS.Types.Model (ModelIdentifier)
import CESDS.Types.Record (VarValue)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Lens.Lens (Lens', lens)
import Data.Maybe (fromJust, fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Packed, Repeated, Required, Value, getField, putField)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)


-- | A unique identifier for a request.
type RequestIdentifier = Word32


-- | A request to a server.
data Request =
  Request
  {
    requestVersion'    :: Required 1 (Value   VersionIdentifier)
  , identifier'        :: Optional 2 (Message OptionalUInt32   )
  , subscribe'         :: Optional 3 (Value   Bool             )
  , loadModelsMeta'    :: Optional 4 (Message LoadModelsMeta   )
  , loadRecordsData'   :: Optional 5 (Message LoadRecordsData  )
  , loadBookmarkMeta'  :: Optional 6 (Message LoadBookmarkMeta )
  , saveBookmarkMeta'  :: Optional 7 (Message SaveBookmarkMeta )
  , cancel'            :: Optional 8 (Message Cancel           )
  , work'              :: Optional 9 (Message Work             )
  }
    deriving (Generic, Show)

instance Decode Request

instance Encode Request


-- | An empty request.
{-# INLINE def #-}
def :: Request
def =
  Request
  {
    requestVersion'   = putField currentVersion
  , identifier'       = mempty
  , subscribe'        = mempty
  , loadModelsMeta'   = mempty
  , loadRecordsData'  = mempty
  , loadBookmarkMeta' = mempty
  , saveBookmarkMeta' = mempty
  , cancel'           = mempty
  , work'             = mempty
  }


-- | Lens for the unique identifier of a request.
identifier :: Lens' Request (Maybe RequestIdentifier)
identifier =
  lens
    (fmap uint32 . getField . identifier')
    (\s x -> s {identifier' = putField $ makeUint32 <$> x})


-- | Lens for whether a request is subscribing to responses.
subscribe :: Lens' Request Bool
subscribe =
  lens
    (fromMaybe False . getField . subscribe')
    (\s x -> s {subscribe' = putField $ Just x})


-- | Handle a request.
onRequest :: Monad m
            => (LoadModelsMeta   -> m a) -- ^ Handler loading models.
            -> (LoadRecordsData  -> m a) -- ^ Handler loading records.
            -> (LoadBookmarkMeta -> m a) -- ^ Handler loading bookmarks.
            -> (SaveBookmarkMeta -> m a) -- ^ Handler saving a bookmark.
            -> (Cancel           -> m a) -- ^ Handler cancelling another request.
            -> (Work             -> m a) -- ^ Handler performing work.
            -> m a                       -- ^ The default result.
            -> Request                   -- ^ The request.
            -> m a                       -- ^ The action to handle the request.
onRequest f g h i j k d Request{..} =
  fmap fromJust
     . sequence
     $  f <$> getField loadModelsMeta'
    <|> g <$> getField loadRecordsData'
    <|> h <$> getField loadBookmarkMeta'
    <|> i <$> getField saveBookmarkMeta'
    <|> j <$> getField cancel'
    <|> k <$> getField work'
    <|> Just d


-- | A request to load model metadata.
data LoadModelsMeta =
  LoadModelsMeta
  {
    loadModelsModelIdentifier' :: Optional 1 (Message OptionalString)
  }
    deriving (Generic, Show)

instance Decode LoadModelsMeta

instance Encode LoadModelsMeta


-- | Construct a request to load the metadata for model(s).  If the model's identifier is not provided, then the request will be for all models.
loadModelsMeta :: Maybe ModelIdentifier -> Request
loadModelsMeta m =
  def
  {
    loadModelsMeta' = putField
                        $ Just 
                          LoadModelsMeta
                          {
                            loadModelsModelIdentifier' = putField $ makeString <$> m
                          }
  }


-- | Handle a request to load model(s).
onLoadModelsMeta :: Monad m
                 => (Maybe ModelIdentifier -> m a) -- ^ Handler loading model(s).
                 -> LoadModelsMeta                 -- ^ The request.
                 -> m a                            -- ^ The action to handle the request.
onLoadModelsMeta f LoadModelsMeta{..} = f (string <$> getField loadModelsModelIdentifier')


-- | A request to load records.
data LoadRecordsData =
  LoadRecordsData
  {
    modelIdentifier'     :: Required 1 (Value   ModelIdentifier   )
  , maxRecords'          :: Optional 2 (Value   Word64            )
  , variableIdentifiers' :: Packed   3 (Value   VariableIdentifier)
  , bookmarkIdentifier'  :: Optional 4 (Value   BookmarkIdentifier)
  , filterExpression'    :: Optional 5 (Message FilterExpression  )
  }
    deriving (Generic, Show)

instance Decode LoadRecordsData

instance Encode LoadRecordsData


-- | Construct a request to load records.
makeLoadRecordsData :: ModelIdentifier          -- ^ The model's identifier.
                    -> Maybe Word64             -- ^ The maximum number of records to return, or all records.
                    -> [VariableIdentifier]     -- ^ The variables to select.
                    -> Maybe BookmarkIdentifier -- ^ Maybe the bookmark to retrieve.
                    -> Maybe FilterExpression   -- ^ Maybe the filter to apply.
                    -> Request                  -- ^ The request.
makeLoadRecordsData m n v b f =
  def
  {
    loadRecordsData' = putField
                         $ Just 
                           LoadRecordsData
                           {
                             modelIdentifier'     = putField m
                           , maxRecords'          = putField n
                           , variableIdentifiers' = putField v
                           , bookmarkIdentifier'  = putField b
                           , filterExpression'    = putField f
                           }
  }


-- | Construct a request to load all records.
loadRecordsData :: ModelIdentifier      -- ^ The model's identifier.
                -> Maybe Word64         -- ^ The maximum number of records to return, or all records.
                -> [VariableIdentifier] -- ^ The variables to select.
                -> Request              -- ^ The request.
loadRecordsData m n v = makeLoadRecordsData m n v Nothing Nothing


-- | Construct a request to load the records for a bookmark.
loadBookmarkedRecordsData :: ModelIdentifier      -- ^ The model's identifier.
                          -> Maybe Word64         -- ^ The maximum number of records to return, or all records.
                          -> [VariableIdentifier] -- ^ The variables to select.
                          -> BookmarkIdentifier   -- ^ The unique identifier for the bookmark.
                          -> Request              -- ^ The request.
loadBookmarkedRecordsData m n v b = makeLoadRecordsData m n v (Just b) Nothing


-- | Construct a request to load the records satisfying a filter.
loadFilteredRecordsData :: ModelIdentifier      -- ^ The model's identifier.
                        -> Maybe Word64         -- ^ The maximum number of records to return, or all records.
                        -> [VariableIdentifier] -- ^ The variables to select.
                        -> Filter               -- ^ The filter.
                        -> Request              -- ^ The request.
loadFilteredRecordsData m n v f = makeLoadRecordsData m n v Nothing (Just $ toExpression f)


-- | Handle a request to load records.
onLoadRecordsData :: Monad m
                  => (ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> m a)                       -- ^ Handle loading all records.
                  -> (ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> BookmarkIdentifier -> m a) -- ^ Handle loading bookmarked records.
                  -> (ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Filter -> m a)             -- ^ Handle loading filtered recrods.
                  -> LoadRecordsData                                                                        -- ^ The request.
                  -> m a                                                                                    -- ^ The action for handling the request.
onLoadRecordsData f g h LoadRecordsData{..} =
  let
    i = getField modelIdentifier'
    n = getField maxRecords'
    v = getField variableIdentifiers'
  in
    fromMaybe (f i n v)
       $  g i n v                  <$> getField bookmarkIdentifier'
      <|> h i n v . fromExpression <$> getField filterExpression'


-- | Handle a request to load records.
onLoadRecordsData' :: Monad m
                  => (ModelIdentifier -> Maybe Word64 -> [VariableIdentifier] -> Maybe (Either BookmarkIdentifier Filter) -> m a) -- ^ Handle loading records.
                  -> LoadRecordsData                                                                                              -- ^ The request.
                  -> m a                                                                                                          -- ^ Action to handle the request.
onLoadRecordsData' f =
  onLoadRecordsData
    (\i n v -> f i n v   Nothing        )
    (\i n v -> f i n v . Just    . Left )
    (\i n v -> f i n v . Just    . Right)


-- | A request to load bookmark metadata.
data LoadBookmarkMeta =
  LoadBookmarkMeta
  {
    loadBookmarkModelIdentifier' :: Required 1 (Value ModelIdentifier )
  , loadBookmarkIdentifier'      :: Optional 2 (Message OptionalString)
  }
    deriving (Generic, Show)

instance Decode LoadBookmarkMeta

instance Encode LoadBookmarkMeta


-- | Construct a request to load bookmark metadata.
loadBookmarkMeta :: ModelIdentifier          -- ^ The model's identifier.
                 -> Maybe BookmarkIdentifier -- ^ The bookmark, or all bookmarks.
                 -> Request                  -- ^ The request.
loadBookmarkMeta m b =
  def
  {
    loadBookmarkMeta' = putField
                          $ Just 
                            LoadBookmarkMeta
                            {
                              loadBookmarkModelIdentifier' = putField m
                            , loadBookmarkIdentifier'      = putField $ makeString <$> b
                            }
  }


-- | Handle a request to load bookmark(s).
onLoadBookmarkMeta :: Monad m
                   => (ModelIdentifier -> Maybe BookmarkIdentifier -> m a) -- ^ Handle loading bookmark(s).
                   -> LoadBookmarkMeta                                     -- ^ The request.
                   -> m a                                                  -- ^ The action to handle the request.
onLoadBookmarkMeta f LoadBookmarkMeta{..} = f (getField loadBookmarkModelIdentifier') (string <$> getField loadBookmarkIdentifier')


-- | A request to save a bookmark.
data SaveBookmarkMeta =
  SaveBookmarkMeta
  {
    saveBookmarkModelIdentifier' :: Required 1 (Value   ModelIdentifier)
  , saveBookmark'                :: Required 2 (Message BookmarkMeta   )
  }
    deriving (Generic, Show)

instance Decode SaveBookmarkMeta

instance Encode SaveBookmarkMeta


-- | Construct a request to save a bookmark.
saveBookmarkMeta :: ModelIdentifier -- ^ The model's identifier.
                 -> BookmarkMeta    -- ^ The bookmark's metadata.
                 -> Request         -- ^ The request.
saveBookmarkMeta m b = 
  def
  {
    saveBookmarkMeta' = putField
                          $ Just 
                            SaveBookmarkMeta
                            {
                              saveBookmarkModelIdentifier' = putField m
                            , saveBookmark'                = putField b
                            }
  }


-- | Handle a request to save a bookmark.
onSaveBookmarkMeta :: Monad m
                   => (ModelIdentifier -> BookmarkMeta -> m a) -- ^ Handle saving a bookmark.
                   -> SaveBookmarkMeta                         -- ^ The request.
                   -> m a                                      -- ^ The action for handling the request.
onSaveBookmarkMeta f SaveBookmarkMeta{..} = f (getField saveBookmarkModelIdentifier') (getField saveBookmark')


-- | A request for new work.
data Work =
  Work
  {
    workModel'  :: Required 1 (Value   String  )
  , workInputs' :: Repeated 2 (Message VarValue)
  }
    deriving (Generic, Show)

instance Decode Work

instance Encode Work


-- | Construct a request for new work.
work :: ModelIdentifier -- ^ The model's identifier.
     -> [VarValue]      -- ^ The values of input variables.
     -> Request         -- ^ The request.
work m is =
  def
  {
    work' = putField
              $ Just
                Work
                {
                  workModel'  = putField m
                , workInputs' = putField is
                }
  }


-- | Handle a request for new work.
onWork :: Monad m
       => (ModelIdentifier -> [VarValue] -> m a) -- ^ Handle doing new work.
       -> Work                                   -- ^ The request.
       -> m a                                    -- ^ The action to handle the request.
onWork f Work{..} = f (getField workModel') (getField workInputs')


-- | A request to cancel a previous request.
data Cancel =
  Cancel
  {
    cancelIdentifier' :: Required 1 (Message OptionalUInt32)
  }
    deriving (Generic, Show)

instance Decode Cancel

instance Encode Cancel


-- | Construct a cancelation request.
cancel :: RequestIdentifier -- ^ The identifier for the request to be cancelled.
       -> Request           -- ^ The request.
cancel i =
  def
    {
      cancel' = putField
                  $ Just
                    Cancel
                    {
                      cancelIdentifier' = putField $ makeUint32 i
                    }
    }


-- | Handle a request for cancellation.
onCancel :: Monad m
         => (RequestIdentifier -> m a) -- ^ Handle cancelling.
         -> Cancel                     -- ^ The request.
         -> m a                        -- ^ The action to handle the request.
onCancel f Cancel{..} = f (uint32 $ getField cancelIdentifier')
