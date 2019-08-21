{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for bookmarks, which are named sets of records or a filter for records.

Use 'makeInterval', 'makeSet', or 'makeFilter' to construct a bookmark, and 'identifier', 'name', and 'onBookmarkMeta' to access its contents.  The function 'filterWithBookmark' will filter a set of records, yielding those that satisfy the bookmark.
-}


{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}


module AESD.Types.Bookmark (
-- * Bookmarks
  BookmarkIdentifier
, BookmarkMeta
, makeInterval
, makeSet
, makeFilter
, setIdentifier
, identifier
, name
, onBookmarkMeta
, filterWithBookmark
-- * Collections of bookmarks
, BookmarkMetas
, makeBookmarks
, bookmarks
) where


import AESD.Types.Filter (Filter, FilterExpression, filterRecords, fromExpression, toExpression)
import AESD.Types.Internal ()
import AESD.Types.Record (RecordContent, RecordIdentifier)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Packed, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


-- | Unique identifier for a bookmark.
type BookmarkIdentifier = String


-- | Metadata for a bookmark.
data BookmarkMeta =
  BookmarkMeta
  {
    identifier'      :: Optional 1 (Value BookmarkIdentifier)
  , name'            :: Required 2 (Value String            )
  , intervalContent' :: Optional 3 (Message IntervalContent )
  , setContent'      :: Optional 4 (Message SetContent      )
  , filterContent'   :: Optional 5 (Message FilterExpression)
  }
    deriving (Generic, Show)

instance Decode BookmarkMeta

instance Encode BookmarkMeta


-- | Construct a bookmark.
{-# INLINE makeBookmark #-}
makeBookmark :: Maybe BookmarkIdentifier -- ^ The unique identifier for the bookmark, optional for a new bookmark.
             -> String                   -- ^ The name for the bookmark.
             -> Maybe IntervalContent    -- ^ Maybe the interval for the record identifiers in the bookmark.
             -> Maybe SetContent         -- ^ Maybe the set of record identifiers in the bookmark.
             -> Maybe FilterExpression   -- ^ Maybe the expression for filtering records for the bookmark.
             -> BookmarkMeta             -- ^ The bookmark.
makeBookmark identifier'' name'' interval'' set'' filter'' =
  BookmarkMeta
  {
    identifier'      = putField identifier''
  , name'            = putField name''
  , intervalContent' = putField interval''
  , setContent'      = putField set''
  , filterContent'   = putField filter''
  }


-- | Construct a bookmark for an interval of record identifiers.
makeInterval :: Maybe BookmarkIdentifier                         -- ^ The unique identifier for the bookmark, optional for a new bookmark.
             -> String                                           -- ^ The name for the bookmark.
             -> (Maybe RecordIdentifier, Maybe RecordIdentifier) -- ^ The interval for the record identifiers in the bookmark.  The interval may be open on either side of the interval.
             -> BookmarkMeta                                     -- ^ The bookmark.
makeInterval identifier'' name'' (firstRecord'', lastRecord'') =
  makeBookmark identifier'' name''
    (
      Just
        IntervalContent
        {
          firstRecord' = putField firstRecord''
        , lastRecord'  = putField lastRecord''
        }
    )
    Nothing
    Nothing


-- | Construct a bookmark for a set of record identifiers.
makeSet :: Maybe BookmarkIdentifier -- ^ The unique identifier for the bookmark, optional for a new bookmark.
        -> String                   -- ^ The name for the bookmark.
        -> [RecordIdentifier]       -- ^ The set of record identifiers in the bookmark.
        -> BookmarkMeta             -- ^ The bookmark.
makeSet identifier'' name'' records'' =
  makeBookmark identifier'' name''
    Nothing
    (
      Just
        SetContent
        {
          setIdentifiers' = putField $ nub records''
        }
    )
    Nothing


-- | Construct a bookmark for a filter.
makeFilter :: Maybe BookmarkIdentifier -- ^ The unique identifier for the bookmark, optional for a new bookmark.
           -> String                   -- ^ The name for the bookmark.
           -> Filter                   -- ^ The expression for filtering records for the bookmark.
           -> BookmarkMeta             -- ^ The bookmark.
makeFilter identifier'' name'' filter'' =
  makeBookmark identifier'' name''
    Nothing
    Nothing
    (Just $ toExpression filter'')
                           

-- | Set the unique identifier of a bookmark.  This is useful for adding the identifier to a new bookmark supplied by a client, but lacking a unique identifier.
{-# INLINE setIdentifier #-}
setIdentifier :: BookmarkIdentifier -> BookmarkMeta -> BookmarkMeta
setIdentifier i b = b {identifier' = putField $ Just i}


-- | Get the unique identifier of a bookmark.
{-# INLINE identifier #-}
identifier :: BookmarkMeta -> Maybe BookmarkIdentifier
identifier = getField . identifier'


-- | Get the name of a bookmark.
{-# INLINE name #-}
name :: BookmarkMeta -> String
name = getField . name'


-- | Apply functions to a bookmark.  A valid bookmark will only specify an interval, set, or filter, so only one of the handler functions will be applied.
--
-- For example:
--
-- >>> import AESD.Types.Bookmark (makeSet, onBookmarkMeta)
-- >>>
-- >>> let b = makeSet (Just "42") "sample set bookmark" [1066, 1812, 1939]
-- >>> onBookmarkMeta (const "interval") (const "set") (const "filter") "unknown" b
-- "set"
-- >>> onBookmarkMeta (const []) id (const []) [] b
-- [1066,1812,1939]
onBookmarkMeta :: ((Maybe RecordIdentifier, Maybe RecordIdentifier) -> a) -- ^ Handle an interval of record identifiers.
               -> ([RecordIdentifier] -> a)                               -- ^ Handle a set of record identifiers.
               -> (Filter -> a)                                           -- ^ Handle a filter.
               -> a                                                       -- ^ Default result.
               -> BookmarkMeta                                            -- ^ The bookmark.
               -> a                                                       -- ^ The result of applying the first applicable handler to the bookmark.
onBookmarkMeta g h f d x =
  fromMaybe d
     $  g . (getField . firstRecord' &&& getField . lastRecord') <$> getField (intervalContent' x)
    <|> h . getField . setIdentifiers'                           <$> getField (setContent'      x)
    <|> f . fromExpression                                       <$> getField (filterContent'   x)


-- | Find the records satisfying a bookmark.
{-# INLINE filterWithBookmark #-}
filterWithBookmark :: BookmarkMeta    -- ^ The bookmark.
                   -> [RecordContent] -- ^ The original records.
                   -> [RecordContent] -- ^ The records satisfying the bookmark.
filterWithBookmark =
  onBookmarkMeta
    filterInterval
    filterSet
    filterRecords
    id


-- | An interval of record identifiers.
data IntervalContent =
  IntervalContent
  {
    firstRecord' :: Optional 1 (Value RecordIdentifier)
  , lastRecord'  :: Optional 2 (Value RecordIdentifier)
  }
    deriving (Generic, Show)

instance Decode IntervalContent

instance Encode IntervalContent


-- | Filter records in an interval of record identifiers.
filterInterval :: (Maybe RecordIdentifier, Maybe RecordIdentifier) -- ^ The interval of record identifiers, possibly open.
               -> [RecordContent]                                  -- ^ The original records.
               -> [RecordContent]                                  -- ^ The records satisfying the bookmark.
filterInterval (firstRecord, lastRecord) =
  filter
    $ liftA2 (&&)
      (>= fromMaybe minBound firstRecord)
      (<= fromMaybe maxBound lastRecord )
    . fst


-- | A set of record identifiers.
data SetContent =
  SetContent
  {
     setIdentifiers' :: Packed 1 (Value RecordIdentifier)
  }
    deriving (Generic, Show)

instance Decode SetContent

instance Encode SetContent


-- | Filter records in a set of record identifiers.
filterSet :: [RecordIdentifier] -- ^ The set of record identifiers.
          -> [RecordContent]    -- ^ The original records.
          -> [RecordContent]    -- ^ The records satisfying the bookmark.
filterSet set = filter $ (`elem` set) . fst


-- | A collections of bookmarks.
data BookmarkMetas =
  BookmarkMetas
  {
     bookmarks' :: Repeated 1 (Message BookmarkMeta)
  }
    deriving (Generic, Show)

instance Decode BookmarkMetas

instance Encode BookmarkMetas


-- | Construct a collection of bookmarks.
{-# INLINE makeBookmarks #-}
makeBookmarks :: [BookmarkMeta] -> BookmarkMetas
makeBookmarks bookmarks'' =
  BookmarkMetas
  {
    bookmarks' = putField bookmarks''
  }


-- | Get the bookmarks in a collection.
{-# INLINE bookmarks #-}
bookmarks :: BookmarkMetas -> [BookmarkMeta]
bookmarks = getField . bookmarks'
