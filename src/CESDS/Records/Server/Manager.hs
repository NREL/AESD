{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Records.Server.Manager ( -- FIXME: Should we export less?
  InMemoryManager
, makeInMemoryManager
, Cache
, ModelCache
, modelMeta
, recordContent
, bookmarkMetas
, contentStatus
, ContentStatus(..)
) where


import CESDS.Records.Server (ModelManager(..), ServiceM, fromService, modifyService, modifyService')
import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta, filterBookmark, identifier)
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


type Cache = Map ModelIdentifier ModelCache


data InMemoryManager a =
  InMemoryManager
  {
    cache'       :: Cache
  , nextBookmark :: IO BookmarkIdentifier
  , persistence  :: Maybe FilePath
  , journal      :: Maybe FileJournal
  , state        :: a
  , lister       :: a -> IO ([ModelMeta], a)                  -- FIXME: Run this in ServerM instead of IO, and supply a function to modify state.
  , loader       :: a -> ModelMeta -> IO ([RecordContent], a) -- FIXME: Run this in ServerM instead of IO, and supply a function to modify state.
  , worker       :: a -> ModelMeta -> [VarValue] -> IO ([RecordContent], a) -- FIXME: Run this in ServerM instead of IO, and supply a function to modify state.
  }

instance ModelManager (InMemoryManager a) where
  listModels =
    do
      InMemoryManager{..} <- fromService id
      (models, state') <- guardSomeException $ lister state
      let
        first = M.null cache'
        additions =
          M.fromList
            $ ((^. Model.identifier) &&& flip (modelMeta .~) def)
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
             Just (Left  bookmark) -> filterBookmark <$> lookupBookmark (model ^. Model.identifier) bookmark
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
              case bookmark ^. Bookmark.identifier of
                Nothing -> return $ bookmark & Bookmark.identifier .~ Just i
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
                          M.insert (fromJust $ b ^. Bookmark.identifier) b
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


checkModel :: ModelIdentifier -> ServiceM (InMemoryManager a) ModelCache
checkModel model =
  maybe (throwError $ "Model \"" ++ model ++ "\" not found.") return
    =<< fromService (M.lookup model . (^. cache))


cache :: Lens' (InMemoryManager a) Cache
cache = lens cache' (\s x -> s {cache' = x})


makeInMemoryManager :: Maybe FilePath -> a -> (a -> IO ([ModelMeta], a)) -> (a -> ModelMeta -> IO ([RecordContent], a)) -> (a -> ModelMeta -> [VarValue] -> IO ([RecordContent], a)) -> IO (InMemoryManager a)
makeInMemoryManager persistence state lister loader worker =
  do
    let
      cache' = M.empty 
      nextBookmark = toString <$> nextRandom
    journal <- sequence $ openJournalIO <$> persistence
    return InMemoryManager{..}


loadBookmarks :: ServiceM (InMemoryManager a) ()
loadBookmarks =
  do
    mbs <- unjournalBookmarks
    let
      z =
        M.fromList
          $ mapReduce
            (second ((,) =<< fromJust . (^. Bookmark.identifier)))
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


journalBookmark :: (ModelIdentifier, BookmarkMeta) -> ServiceM (InMemoryManager a) ()
journalBookmark (model, bookmark) =
  do
    journal' <- fromService journal
    forM_ journal'
      $ flip append
      (
        encode (model, fromJust $ bookmark ^. Bookmark.identifier)
      , runPut $ encodeMessage bookmark
      )


data ModelCache =
  ModelCache
  {
    modelMeta'     :: ModelMeta
  , recordContent' :: [RecordContent]
  , contentStatus' :: ContentStatus
  , bookmarkMetas' :: Map BookmarkIdentifier BookmarkMeta
  }
    deriving (Generic, Show)

instance Default ModelCache where
  def = ModelCache def [] def M.empty

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


modelMeta :: Lens' ModelCache ModelMeta
modelMeta = lens modelMeta' (\s x -> s {modelMeta' = x})


recordContent :: Lens' ModelCache [RecordContent]
recordContent = lens recordContent' (\s x -> s {recordContent' = x})


contentStatus :: Lens' ModelCache ContentStatus
contentStatus = lens contentStatus' (\s x -> s {contentStatus' = x})


bookmarkMetas :: Lens' ModelCache (Map BookmarkIdentifier BookmarkMeta)
bookmarkMetas = lens bookmarkMetas' (\s x -> s {bookmarkMetas' = x})


data ContentStatus =
    EmptyContent
  | PendingContent
  | CompleteContent
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Default ContentStatus where
  def = EmptyContent
