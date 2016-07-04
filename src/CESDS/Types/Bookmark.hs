{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Bookmark (
  BookmarkIdentifier
, Bookmark(..)
, validateBookmarks
, validateBookmark
) where


import CESDS.Types (Color, Identifier, Tags, object')
import CESDS.Types.Record (RecordIdentifier)
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import Data.List.Util (deleteOn, hasSubset, noDuplicates, notDuplicatedIn)
import Data.Maybe (fromMaybe)
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
  , tags       :: Tags
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
                      size       <- o' .:  "size"
                      color      <- o' .:? "color"
                      tags       <- o' .:  "tags"
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
                    , "size"        .= size
                    , "color"       .= color
                    , "tags"        .= tags
                    ]
      ]


validateBookmarks :: (IsString e, MonadError e m) => [RecordIdentifier] -> [Bookmark] -> m ()
validateBookmarks recordIdentifiers bookmarks =
  do
    unless (noDuplicates $ map identifier bookmarks)
      $ throwError "duplicate bookmark identifiers"
    sequence_
      [
        validateBookmark (deleteOn identifier bookmark bookmarks) recordIdentifiers bookmark
      |
        bookmark <-  bookmarks
      ]


validateBookmark :: (IsString e, MonadError e m) => [Bookmark] -> [RecordIdentifier]-> Bookmark -> m ()
validateBookmark bookmarks recordIdentifiers bookmark =
  do
    unless (notDuplicatedIn identifier bookmark bookmarks)
      $ throwError "duplicate bookmark identifiers"
    unless (maybe False (not . null) $ records bookmark)
      $ throwError "no record identifiers in bookmark"
    unless (size bookmark == maybe 0 length (records bookmark))
      $ throwError "incorrect bookmark size"
    unless (recordIdentifiers `hasSubset` fromMaybe [] (records bookmark))
      $ throwError "invalid record identifiers"
