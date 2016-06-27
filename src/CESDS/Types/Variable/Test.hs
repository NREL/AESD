{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Variable.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Variable (Display(..), Domain(..), Variable(..), Units(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof)


instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

 
instance Arbitrary Display where
  arbitrary = Display <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary Domain where
  arbitrary =
    oneof
      [
        Interval <$> arbitrary <*> arbitrary
      , Set <$> arbitrary
      ]


instance Arbitrary Units where
  arbitrary = Units <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
