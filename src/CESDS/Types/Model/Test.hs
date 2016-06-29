{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Model.Test (
  arbitraryModel
) where


import CESDS.Types.Test ()
import CESDS.Types.Model (Model(..), ModelIdentifier)
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, elements, listOf1, resize)


arbitraryModel :: ModelIdentifier -> Gen Model
arbitraryModel identifier =
    do
      uri         <- arbitrary
      name        <- arbitrary
      description <- arbitrary
      tags        <- arbitrary
      generation  <- arbitrary
      variables   <- nub <$> resize 4 (listOf1 arbitrary)
      primaryKey  <- elements variables
      timeKey     <- elements $ Nothing : map Just variables
      return Model{..}

    
instance Arbitrary Model where
  arbitrary = arbitraryModel =<< arbitrary
