{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Bookmark.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Bookmark (Bookmark(..))
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof, resize)


instance Arbitrary Bookmark where
  arbitrary =
    Bookmark
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof [return Nothing, Just . nub <$> resize 4 arbitrary]
