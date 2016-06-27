{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Model.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Model (Model(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, listOf1)


instance Arbitrary Model where
  arbitrary =
    do
      identifier  <- arbitrary
      uri         <- arbitrary
      name        <- arbitrary
      description <- arbitrary
      tags        <- arbitrary
      generation  <- arbitrary
      variables   <- listOf1 arbitrary
      primaryKey  <- elements variables
      timeKey     <- elements $ Nothing : map Just variables
      return Model{..}
