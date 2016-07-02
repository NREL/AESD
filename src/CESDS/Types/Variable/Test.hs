{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Variable.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Variable (Display(..), Domain(..), Variable(..), Units(..), isSet)
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (listOf1, oneof, suchThat)
 
 
instance Arbitrary Variable where
  arbitrary =
    do
      identifier <- arbitrary
      display    <- arbitrary
      domain     <- arbitrary
      units      <- if isSet domain then return Nothing else arbitrary
      isInput    <- arbitrary
      return Variable{..}

 
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
      , Set . fmap nub <$> oneof [return Nothing, Just <$>listOf1 arbitrary]
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
