{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Bookmark (
  BookmarkIdentifier
, Bookmark(..)
) where


import CESDS.Types (Color, Identifier, Tags)
import CESDS.Types.Result (ResultIdentifier)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


type BookmarkIdentifier = Identifier


data Bookmark =
  Bookmark
  {
    identifer :: BookmarkIdentifier
  , name      :: Text
  , size      :: Int
  , color     :: Maybe Color
  , tags      :: Tags
  , results   :: Maybe [ResultIdentifier]
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
