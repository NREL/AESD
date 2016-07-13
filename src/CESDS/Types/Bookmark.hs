{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Bookmark (
  BookmarkIdentifier
, Bookmark(..)
, validateBookmark
, validateBookmarks
, BookmarkList(..)
, makeBookmarkList
, validateBookmarkList
) where


import CESDS.Types (Color, Identifier, Tags, object')
import CESDS.Types.Record (RecordIdentifier)
import Control.Monad.Except (MonadError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import Data.List.Util (deleteOn, hasSubset, noDuplicates, notDuplicatedIn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)


type BookmarkIdentifier = Identifier


data Bookmark =
  Bookmark
  {
    identifier :: Maybe BookmarkIdentifier
  , name       :: Text
  , size       :: Int
  , color      :: Maybe Color
  , tags       :: Maybe Tags
  , records    :: Maybe [RecordIdentifier]
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Bookmark where
  parseJSON =
    withObject "BOOKMARK" $ \o ->
      do
        meta <- withObject "BOOKMARK_META"
                  (\o' ->
                    do
                      identifier <- o' .:? "bookmark_id"
                      name       <- o' .:  "name"
                      size       <- o' .:  "count"
                      color      <- o' .:? "color"
                      tags       <- o' .:? "tags"
                      let records  = Nothing
                      return Bookmark{..}
                  )
                  =<< o .: "meta"
        records <- o .:? "record_ids"
        return $ meta {records = records}

instance ToJSON Bookmark where
  toJSON Bookmark{..} =
    object'
      $ maybe id ((:) . ("record_ids" .=)) records
      [
        "meta" .= object'
                    [
                      "bookmark_id" .= identifier
                    , "name"        .= name
                    , "count"       .= size
                    , "color"       .= color
                    , "tags"        .= tags
                    ]
      ]


validateBookmark :: (IsString e, MonadError e m) => [Bookmark] -> [RecordIdentifier]-> Bookmark -> m ()
validateBookmark bookmarks recordIdentifiers bookmark =
  do
    assert "duplicate bookmark identifiers" $ notDuplicatedIn identifier bookmark bookmarks
    assert "no record identifiers in bookmark" $ maybe False (not . null) $ records bookmark
    assert "incorrect bookmark size" $ size bookmark == maybe 0 length (records bookmark)
    assert "invalid record identifiers" $ recordIdentifiers `hasSubset` fromMaybe [] (records bookmark)


validateBookmarks :: (IsString e, MonadError e m) => [RecordIdentifier] -> [Bookmark] -> m ()
validateBookmarks recordIdentifiers bookmarks =
  do
    assert "duplicate bookmark identifiers" $ noDuplicates $ map identifier bookmarks
    sequence_
      [
        validateBookmark (deleteOn identifier bookmark bookmarks) recordIdentifiers bookmark
      |
        bookmark <-  bookmarks
      ]


data BookmarkList =
  BookmarkList
  {
    count     :: Int
  , bookmarks :: [BookmarkIdentifier]
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON BookmarkList where
  parseJSON =
    withObject "BOOKMARK_META_LIST" $ \o ->
      do
        count <- o .: "count"
        bookmarks <- o .: "bookmark_ids"
        return BookmarkList{..}

instance ToJSON BookmarkList where
  toJSON BookmarkList{..} = object' ["count" .= count, "bookmark_ids" .= bookmarks]


makeBookmarkList :: [Bookmark] -> BookmarkList
makeBookmarkList items =
  let
    bookmarks = mapMaybe identifier items
    count = length bookmarks
  in
    BookmarkList{..}


validateBookmarkList :: (IsString e, MonadError e m) => BookmarkList -> m ()
validateBookmarkList BookmarkList{..}  =
  do
    assert "bookmark count does not match number of bookmarks" $ count == length bookmarks
    assert "duplicate bookmark identifiers" $ noDuplicates bookmarks
