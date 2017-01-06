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


import CESDS.Records.Server (ModelManager(..), ServiceM, fromService, guardIO, modifyService, modifyService')
import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta, filterBookmark, identifier)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier)
import CESDS.Types.Record (RecordContent, filterVariables)
import Control.Arrow ((&&&))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~), over)
import Control.Monad (forM_, void)
import Control.Monad.Except (liftIO, throwError)
import Data.ProtocolBuffers (decodeMessage, encodeMessage)
import Data.Default (Default(..))
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Serialize (Serialize(..), decodeLazy, encodeLazy)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)

import qualified Data.ByteString.Lazy as BS (readFile)
import qualified Data.Map.Strict as M (elems, empty, fromList, insert, lookup, member, size, toList, union, update)


type Cache = Map ModelIdentifier ModelCache


data InMemoryManager a =
  InMemoryManager
  {
    cache'       :: Cache
  , nextBookmark :: IO BookmarkIdentifier
  , persistence  :: Maybe FilePath
  , state        :: a
  , lister       :: a -> IO ([ModelMeta], a)                  -- FIXME: Run this in ServerM instead of IO, and supply a function to modify state.
  , loader       :: a -> ModelMeta -> IO ([RecordContent], a) -- FIXME: Run this in ServerM instead of IO, and supply a function to modify state.
  }

instance ModelManager (InMemoryManager a) where
  listModels =
    do
      InMemoryManager{..} <- fromService id
      (models, state') <- guardIO $ lister state
      let
        additions =
          M.fromList
            $ ((^. Model.identifier) &&& flip (modelMeta .~) def)
            <$> models
        -- FIXME: Should we also delete models not longer present?
      modifyService
        $ (\c -> c {state = state'}) -- FIXME: Create a lens for state.
        . over cache (`M.union` additions)
      return models
  lookupModel model =
    (^. modelMeta)
      <$> checkModel model
  loadContent model maybeBookmark variables =
    do -- FIXME: Should we also cache the records?
      f <- maybe (return id) (fmap filterBookmark . lookupBookmark (model ^. Model.identifier)) maybeBookmark
      InMemoryManager{..} <- fromService id
      (records, state') <- guardIO $ loader state model
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
      void $ fromService saveManager -- FIXME: This would perform poorly if there are a lot of models or bookmarks.
      return bookmark'


checkModel :: ModelIdentifier -> ServiceM (InMemoryManager a) ModelCache
checkModel model =
  maybe (throwError "Model not found.") return
    =<< fromService (M.lookup model . (^. cache))


cache :: Lens' (InMemoryManager a) Cache
cache = lens cache' (\s x -> s {cache' = x})


makeInMemoryManager :: Maybe FilePath -> a -> (a -> IO ([ModelMeta], a)) -> (a -> ModelMeta -> IO ([RecordContent], a)) -> IO (InMemoryManager a)
makeInMemoryManager persistence state lister loader =
  do
    let
      cache' = M.empty 
      nextBookmark = toString <$> nextRandom
    restoreManager InMemoryManager{..}


restoreManager :: InMemoryManager a -> IO (InMemoryManager a)
restoreManager m@InMemoryManager{..} =
 maybe
   (return m)
   (
     \file ->
       do
         Right x <- decodeLazy <$> BS.readFile file -- FIXME
         return m {cache' = x}
   )
   persistence


saveManager :: InMemoryManager a -> IO ()
saveManager InMemoryManager{..} =
  forM_ persistence
    . flip atomicWriteFile
    $ encodeLazy cache'


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
