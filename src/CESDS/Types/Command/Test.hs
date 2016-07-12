{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Command.Test (
) where


import CESDS.Types.Test (arbitrarySimpleJSON)
import CESDS.Types.Command (Command(..), Result(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof, listOf, resize)


instance Arbitrary Command where
  arbitrary =
    oneof
      $ map (<$> resize 4 (listOf arbitrarySimpleJSON))
      [Restart, Clear, SetStrategy, GetStrategy]


instance Arbitrary Result where
  arbitrary = Result <$> arbitrary
