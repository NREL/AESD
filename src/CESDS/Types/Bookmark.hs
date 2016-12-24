{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}


module CESDS.Types.Bookmark (
  BookmarkIdentifier
, BookmarkMeta
, identifier
, name
, numberOfRecords
, intervalContent
, setContent
, onBookmarkMeta
, IntervalContent
, firstRecord
, lastRecord
, SetContent
, setIdentifiers
, BookmarkMetas
, bookmarks
) where


import CESDS.Types.Internal ()
import CESDS.Types.Record (RecordIdentifier)
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Data.Default (Default(..))
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Packed, Repeated, Required, Value, getField, putField)
import Data.Word (Word64)
import GHC.Generics (Generic)


type BookmarkIdentifier = String


data IntervalContent =
  IntervalContent
  {
    firstRecord' :: Optional 1 (Value RecordIdentifier)
  , lastRecord'  :: Optional 2 (Value RecordIdentifier)
  }
    deriving (Generic, Show)

instance Default IntervalContent where
  def = IntervalContent (putField Nothing) (putField Nothing)

instance Decode IntervalContent

instance Encode IntervalContent


firstRecord :: Lens' IntervalContent (Maybe RecordIdentifier)
firstRecord = lens (getField . firstRecord') (\s x -> s {firstRecord' = putField x})


lastRecord :: Lens' IntervalContent (Maybe RecordIdentifier)
lastRecord = lens (getField . lastRecord') (\s x -> s {lastRecord' = putField x})


data SetContent =
  SetContent
  {
     setIdentifiers' :: Packed 1 (Value RecordIdentifier)
  }
    deriving (Generic, Show)

instance Default SetContent where
  def = SetContent $ putField []

instance Decode SetContent

instance Encode SetContent


setIdentifiers :: Lens' SetContent [RecordIdentifier]
setIdentifiers = lens (getField . setIdentifiers') (\s x -> s {setIdentifiers' = putField x})


data BookmarkMeta =
  BookmarkMeta
  {
    identifier'      :: Optional 1 (Value BookmarkIdentifier)
  , name'            :: Required 2 (Value String            )
  , numberOfRecords' :: Optional 3 (Value Word64            )
  , intervalContent' :: Optional 4 (Message IntervalContent )
  , setContent'      :: Optional 5 (Message SetContent      )
  }
    deriving (Generic, Show)

instance Default BookmarkMeta where
  def =
    BookmarkMeta
    {
      identifier'      = putField Nothing
    , name'            = putField ""
    , numberOfRecords' = putField Nothing
    , intervalContent' = putField Nothing
    , setContent'      = putField Nothing
    }

instance Decode BookmarkMeta

instance Encode BookmarkMeta


identifier :: Lens' BookmarkMeta (Maybe BookmarkIdentifier)
identifier = lens (getField . identifier') (\s x -> s {identifier' = putField x})


name :: Lens' BookmarkMeta String
name = lens (getField . name') (\s x -> s {name' = putField x})


numberOfRecords :: Lens' BookmarkMeta (Maybe Word64)
numberOfRecords = lens (getField . numberOfRecords') (\s x -> s {numberOfRecords' = putField x})


intervalContent :: Lens' BookmarkMeta (Maybe IntervalContent)
intervalContent = lens (getField . intervalContent') (\s x -> s {intervalContent' = putField x})


setContent :: Lens' BookmarkMeta (Maybe SetContent)
setContent = lens (getField . setContent') (\s x -> s {setContent' = putField x})


onBookmarkMeta :: (Word64 -> a) -> (IntervalContent -> a) -> (SetContent -> a) -> BookmarkMeta -> Maybe a
onBookmarkMeta f g h x =
      f <$> x ^. numberOfRecords
  <|> g <$> x ^. intervalContent
  <|> h <$> x ^. setContent


data BookmarkMetas =
  BookmarkMetas
  {
     bookmarks' :: Repeated 1 (Message BookmarkMeta)
  }
    deriving (Generic, Show)

instance Default BookmarkMetas where
  def = BookmarkMetas $ putField []

instance Decode BookmarkMetas

instance Encode BookmarkMetas


bookmarks :: Lens' BookmarkMetas [BookmarkMeta]
bookmarks = lens (getField . bookmarks') (\s x -> s {bookmarks' = putField x})
