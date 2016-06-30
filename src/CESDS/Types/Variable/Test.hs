{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Variable.Test (
  arbitraryVariable
) where


import CESDS.Types.Test ()
import CESDS.Types.Variable (Display(..), Domain(..), Variable(..), VariableIdentifier, Units(..))
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, oneof, suchThat)
 
 
arbitraryVariable :: VariableIdentifier -> Gen Variable
arbitraryVariable identifier =
  do
    display <- arbitrary
    domain  <- arbitrary
    units   <- arbitrary
    isInput <- arbitrary
    return Variable{..}

 
instance Arbitrary Variable where
  arbitrary = arbitraryVariable =<< arbitrary

 
instance Arbitrary Display where
  arbitrary =
    Display
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary


instance Arbitrary Domain where
  arbitrary =
    oneof
      [
        uncurry Interval <$> suchThat arbitrary ordered
      , Set . fmap nub <$> arbitrary
      ]
    where
      ordered (Just l, Just u) = l < u
      ordered _                = True


instance Arbitrary Units where
  arbitrary =
    Units
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> suchThat arbitrary (/= 0)
