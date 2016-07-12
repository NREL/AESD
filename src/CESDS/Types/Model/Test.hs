{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Model.Test (
  arbitraryModel
) where


import CESDS.Types.Test ()
import CESDS.Types.Model (Model(..), ModelIdentifier)
import CESDS.Types.Variable.Test ()
import Data.Function (on)
import Data.List (nubBy)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, elements, listOf1, resize, suchThat)

import qualified CESDS.Types.Variable as Variable (Variable(..))


arbitraryModel :: ModelIdentifier -> Gen Model
arbitraryModel identifier =
    do
      uri         <- arbitrary
      name        <- arbitrary
      description <- arbitrary
      tags        <- arbitrary
      generation  <- arbitrary
      recordCount <- arbitrary `suchThat` (>= 0)
      variables   <- nubBy ((==) `on` Variable.identifier) <$> resize 4 (listOf1 arbitrary)
      primaryKey  <- elements $ map Variable.identifier variables
      timeKey     <- elements $ Nothing : map (Just . Variable.identifier) variables
      return Model{..}

    
instance Arbitrary Model where
  arbitrary = arbitraryModel =<< arbitrary
