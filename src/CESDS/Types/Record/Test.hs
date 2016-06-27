{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Record.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Record (Record(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))


instance Arbitrary Record where
  arbitrary = Record <$> arbitrary
