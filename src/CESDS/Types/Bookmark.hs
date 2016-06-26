{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Bookmark (
  BookmarkIdentifier
, Bookmark(..)
) where


import CESDS.Types (Color, Identifier, Tags)
import CESDS.Types.Result (ResultIdentifier)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.=), object, withObject)
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
  , results    :: Maybe [ResultIdentifier]
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Bookmark where
  parseJSON =
    withObject "BOOKMARK" $ \o ->
      do
        meta <- withObject "BOOKMARK_META"
                  (\o' ->
                    do
                      identifier <- o' .: "bookmark_id"
                      name       <- o' .: "name"
                      size       <- o' .: "size"
                      color      <- o' .: "color"
                      tags       <- o' .: "tags"
                      let results  = Nothing
                      return Bookmark{..}
                  )
                  =<< o .: "meta"
        results <- o .: "record_ids"
        return $ meta {results = results}

instance ToJSON Bookmark where
  toJSON Bookmark{..} =
    object
      $ maybe id ((:) . ("record_ids" .=)) results
      [
        "meta" .= object
                    [
                      "bookmark_id" .= identifier
                    , "name"        .= name
                    , "size"        .= size
                    , "color"       .= color
                    , "tags"        .= tags
                    ]
      ]
