{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Bookmark.Test (
  arbitraryBookmark
) where


import CESDS.Types.Test ()
import CESDS.Types.Bookmark (Bookmark(..), BookmarkIdentifier)
import CESDS.Types.Record (RecordIdentifier)
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, oneof, resize, sublistOf, suchThat)


arbitraryBookmark :: [Maybe BookmarkIdentifier] -> [RecordIdentifier] -> Gen Bookmark
arbitraryBookmark veto recordIdentifiers =
  do
    identifier <- arbitrary `suchThat` (`notElem` veto)
    name       <- arbitrary
    records    <- oneof [return Nothing, Just . nub <$> resize 4 (sublistOf recordIdentifiers)]
    color      <- arbitrary
    tags       <- arbitrary
    let size   = length records 
    return Bookmark{..}

instance Arbitrary Bookmark where
  arbitrary = arbitraryBookmark [] =<< arbitrary
