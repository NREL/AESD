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
, filterBookmark
, BookmarkMetas
, bookmarks
) where


import CESDS.Types.Internal ()
import CESDS.Types.Record (RecordContent, RecordIdentifier)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
import Control.Monad (liftM2)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
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
  def = IntervalContent (putField def) (putField def)

instance Decode IntervalContent

instance Encode IntervalContent


intervalIdentifiers :: Lens' IntervalContent (RecordIdentifier, RecordIdentifier)
intervalIdentifiers =
  lens
    (fromMaybe minBound . getField . firstRecord' &&& fromMaybe maxBound . getField . lastRecord')
    (\s (x, y) -> s {firstRecord' = putField $ Just x, lastRecord' = putField $ Just y})


filterInterval :: (RecordIdentifier, RecordIdentifier) -> [RecordContent] -> [RecordContent]
filterInterval (firstRecord, lastRecord) =
  filter (liftM2 (&&) (>= firstRecord) (<= lastRecord) . fst)


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


filterSet :: [RecordIdentifier] -> [RecordContent] -> [RecordContent]
filterSet set = filter ((`elem` set) . fst)


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


intervalContent :: Lens' BookmarkMeta (Maybe (RecordIdentifier, RecordIdentifier))
intervalContent =
  lens
    (fmap (^. intervalIdentifiers) . getField . intervalContent')
    (\s x -> s {intervalContent' = putField $ flip (intervalIdentifiers .~) def <$> x})


setContent :: Lens' BookmarkMeta (Maybe [RecordIdentifier])
setContent =
  lens
    (fmap (^. setIdentifiers) . getField . setContent')
    (\s x -> s {setContent' = putField $ flip (setIdentifiers .~) def <$> x})


onBookmarkMeta :: (Word64 -> a) -> ((RecordIdentifier, RecordIdentifier) -> a) -> ([RecordIdentifier] -> a) -> a -> BookmarkMeta -> a
onBookmarkMeta f g h d x =
  fromMaybe d
     $  f <$> x ^. numberOfRecords
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


filterBookmark :: BookmarkMeta -> [RecordContent] -> [RecordContent]
filterBookmark =
  onBookmarkMeta
    (const id)
    filterInterval
    filterSet
    id
