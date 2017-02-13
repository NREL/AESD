{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Support for managing models in memory.
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Records.Server.Manager ( -- FIXME: Should we export less?
-- * Manager
  InMemoryManager
, makeInMemoryManager
-- * Cache
, Cache
, ModelCache
, modelMeta
, recordContent
, bookmarkMetas
-- * Status
, ContentStatus(..)
, contentStatus
) where


import CESDS.Records.Server (ModelManager(..), ServiceM, fromService, modifyService, modifyService')
import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta, filterWithBookmark, identifier, setIdentifier)
import CESDS.Types.Filter (filterRecords)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier)
import CESDS.Types.Record (RecordContent, VarValue, filterVariables)
import Control.Arrow ((&&&), second)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~), over)
import Control.Monad (forM_, when)
import Control.Monad.Except (liftIO, throwError)
import Control.Monad.Except.Util (guardSomeException)
import Data.Default (Default(..))
import Data.Function.MapReduce (mapReduce)
import Data.Journal (Journal(..))
import Data.Journal.File (FileJournal, openJournalIO)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.ProtocolBuffers (decodeMessage, encodeMessage)
import Data.Serialize (Serialize(..), decode, encode, runGet, runPut)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as M (elems, empty, findWithDefault, fromList, insert, lookup, mapWithKey, member, null, size, toList, union, update)


-- | Manager for models in memory.
data InMemoryManager a =
  InMemoryManager
  {
    cache'       :: Cache                                                   -- ^ The cache for models.
  , nextBookmark :: IO BookmarkIdentifier                                   -- ^ Action for creating the next bookmark identifier.
  , persistence  :: Maybe FilePath                                          -- ^ The path to the journal for persistence.
  , journal      :: Maybe FileJournal                                       -- ^ The journal for persistence.
  , state        :: a                                                       -- ^ The state.
  , lister       :: a -> IO ([ModelMeta], a)                                -- ^ Handle listing models.
  , loader       :: a -> ModelMeta -> IO ([RecordContent], a)               -- ^ Handle loading record data.
  , worker       :: a -> ModelMeta -> [VarValue] -> IO ([RecordContent], a) -- ^ Handle performing work.
  } -- FIXME: Run the handlers in ServerM instead of IO, and supply a function to modify state.

instance ModelManager (InMemoryManager a) where

  listModels =
    do
      InMemoryManager{..} <- fromService id
      (models, state') <- guardSomeException $ lister state
      let
        first = M.null cache'
        additions =
          M.fromList
            $ (Model.identifier &&& flip (modelMeta .~) def)
            <$> models
        -- FIXME: Should we also delete models not longer present?
      modifyService
        $ (\c -> c {state = state'}) -- FIXME: Create a lens for state.
        . over cache (`M.union` additions)
      when first loadBookmarks
      return models

  lookupModel model =
    (^. modelMeta)
      <$> checkModel model

  loadContent model maybeBookmarkOrFilter variables =
    do -- FIXME: Should we also cache the records?
      f <- case maybeBookmarkOrFilter of
             Nothing               -> return id
             Just (Left  bookmark) -> filterWithBookmark <$> lookupBookmark (Model.identifier model) bookmark
             Just (Right filtr   ) -> return $ filterRecords filtr
      InMemoryManager{..} <- fromService id
      (records, state') <- guardSomeException $ loader state model
      modifyService
        $ \c -> c {state = state'}
      return
        . (if null variables then id else filterVariables variables)
        $ f records

  listBookmarks model =
    M.elems . (^. bookmarkMetas)
      <$> checkModel model

  lookupBookmark model bookmark =
    maybe (throwError "Bookmark not found.") return
      =<< M.lookup bookmark . (^. bookmarkMetas)
      <$> checkModel model

  saveBookmark model bookmark =
    do
      i <- liftIO =<< fromService nextBookmark
      bookmark' <-
        modifyService'
          $ \manager ->
          do
            m <-
              maybe (throwError "Model not found.") Right
                . M.lookup model
                $ manager ^. cache
            b <-
              case Bookmark.identifier bookmark of
                Nothing -> return $ setIdentifier i bookmark
                Just i' -> if i' `M.member` (m ^. bookmarkMetas) then return bookmark else throwError "Bookmark not found."
            return
              (
                manager
                  & over cache
                  (
                    M.update
                      (
                        Just . over bookmarkMetas
                        (
                          M.insert (fromJust $ Bookmark.identifier b) b
                        )
                      )
                      model
                  )
              , b
              )
      journalBookmark (model, bookmark')
      return bookmark'

  doWork model inputs =
    do -- FIXME: Should we also cache the records?
      InMemoryManager{..} <- fromService id
      (records, state') <- guardSomeException $ worker state model inputs
      modifyService
        $ \c -> c {state = state'}
      return records


-- | Ensure that a model is in the cache.
checkModel :: ModelIdentifier                         -- ^ The model identifier.
           -> ServiceM (InMemoryManager a) ModelCache -- ^ Action to ensure the model is in the cache.
checkModel model =
  maybe (throwError $ "Model \"" ++ model ++ "\" not found.") return
    =<< fromService (M.lookup model . (^. cache))


-- | Construct an in-memory model manager.
makeInMemoryManager :: Maybe FilePath                                            -- ^ The name of the journal file.
                    -> a                                                         -- ^ The initial state.
                    -> (a -> IO ([ModelMeta], a))                                -- ^ Handle listing models.
                    -> (a -> ModelMeta -> IO ([RecordContent], a))               -- ^ Handle loading record data.
                    -> (a -> ModelMeta -> [VarValue] -> IO ([RecordContent], a)) -- ^ Handle performing work.
                    -> IO (InMemoryManager a)                                    -- ^ Action constructing the manager.
makeInMemoryManager persistence state lister loader worker =
  do
    let
      cache' = M.empty 
      nextBookmark = toString <$> nextRandom
    journal <- sequence $ openJournalIO <$> persistence
    return InMemoryManager{..}


-- | Lens for the cache.
cache :: Lens' (InMemoryManager a) Cache
cache = lens cache' (\s x -> s {cache' = x})


-- | Load bookmarks.
loadBookmarks :: ServiceM (InMemoryManager a) ()
loadBookmarks =
  do
    mbs <- unjournalBookmarks
    let
      z =
        M.fromList
          $ mapReduce
            (second ((,) =<< fromJust . Bookmark.identifier))
            ((. M.fromList) . (,))
            mbs :: Map ModelIdentifier (Map BookmarkIdentifier BookmarkMeta)
    modifyService
      $ \m@InMemoryManager{..} ->
        m
        {
          cache' =
            M.mapWithKey
              (
                (bookmarkMetas .~ ) . flip (M.findWithDefault M.empty) z
              )
              cache'
        }


-- | Retrieve journaled bookmarks.
unjournalBookmarks :: ServiceM (InMemoryManager a) [(ModelIdentifier, BookmarkMeta)]
unjournalBookmarks =
  do
    journal' <- fromService journal
    case journal' of
      Nothing           -> return []
      Just    journal'' -> either throwError return
                             . mapM
                             (
                               \(k, e) ->
                                 do
                                   (model, _) <- decode k :: Either String (ModelIdentifier, BookmarkIdentifier)
                                   bookmark <- runGet decodeMessage e
                                   return (model, bookmark)
                             )
                             =<< replay False journal''


-- | Journal a bookmark.
journalBookmark :: (ModelIdentifier, BookmarkMeta) -> ServiceM (InMemoryManager a) ()
journalBookmark (model, bookmark) =
  do
    journal' <- fromService journal
    forM_ journal'
      $ flip append
      (
        encode (model, fromJust $ Bookmark.identifier bookmark)
      , runPut $ encodeMessage bookmark
      )


-- | A cache for models.
type Cache = Map ModelIdentifier ModelCache


-- | A cache for a single model.
data ModelCache =
  ModelCache
  {
    modelMeta'     :: ModelMeta                           -- ^ The model metadata.
  , recordContent' :: [RecordContent]                     -- ^ The record data.
  , contentStatus' :: ContentStatus                       -- ^ The status of content.
  , bookmarkMetas' :: Map BookmarkIdentifier BookmarkMeta -- ^ The bookmark metadata.
  }
    deriving (Generic, Show)

instance Default ModelCache where
  def = ModelCache undefined [] def M.empty

instance Serialize ModelCache where

  get =
    do
      modelMeta' <- decodeMessage
      n <- get
      let
        recordContent' = []
        contentStatus' = EmptyContent
      bookmarkMetas' <-
        M.fromList
          <$> sequence
          [
            do
              k <- get
              v <- decodeMessage
              return (k, v)
          |
            _ <- [1..n] :: [Int]
          ]
      return ModelCache{..}

  put ModelCache{..} =
    do
      encodeMessage modelMeta'
      let
        n = M.size bookmarkMetas'
      put n
      sequence_
        [
          do
            put k
            encodeMessage v
        |
          (k, v) <- M.toList bookmarkMetas'
        ]


-- | Lens for model metadata.
modelMeta :: Lens' ModelCache ModelMeta
modelMeta = lens modelMeta' (\s x -> s {modelMeta' = x})


-- | Lens for record data.
recordContent :: Lens' ModelCache [RecordContent]
recordContent = lens recordContent' (\s x -> s {recordContent' = x})


-- | Lens for bookmarks.
bookmarkMetas :: Lens' ModelCache (Map BookmarkIdentifier BookmarkMeta)
bookmarkMetas = lens bookmarkMetas' (\s x -> s {bookmarkMetas' = x})


-- | Status of record data.
data ContentStatus =
    EmptyContent
  | PendingContent
  | CompleteContent
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Default ContentStatus where
  def = EmptyContent


-- | Lens for status of record data.
contentStatus :: Lens' ModelCache ContentStatus
contentStatus = lens contentStatus' (\s x -> s {contentStatus' = x})
