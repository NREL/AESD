{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Bookmark.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Bookmark (Bookmark(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))


instance Arbitrary Bookmark where
  arbitrary = Bookmark <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
