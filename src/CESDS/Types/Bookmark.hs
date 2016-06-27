{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Bookmark (
  BookmarkIdentifier
, Bookmark(..)
) where


import CESDS.Types (Color, Identifier, Tags, object')
import CESDS.Types.Record (RecordIdentifier)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)


type BookmarkIdentifier = Identifier


data Bookmark =
  Bookmark
  {
    identifier :: BookmarkIdentifier
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
                      identifier <- o' .:  "bookmark_id"
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
