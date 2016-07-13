{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Variable.Test (
  arbitrarySubdomain
) where


import CESDS.Types (Val(Continuous))
import CESDS.Types.Test (arbitraryVal)
import CESDS.Types.Variable (Display(..), Domain(..), Variable(..), Units(..), isSet)
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, elements, frequency, listOf1, sublistOf, suchThat)
 
 
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
    frequency
      [
        (9, uncurry Interval <$> arbitrary `suchThat` ordered)
      , (1, Set . nub <$> listOf1 arbitrary                  )
      ]
    where
      ordered (Just l, Just u) = l < u
      ordered _                = True


arbitrarySubdomain :: Domain -> Gen Domain
arbitrarySubdomain i@Interval{..} =
  do
    Continuous x0 <- arbitraryVal i
    Continuous x1 <- arbitraryVal i
    let
      xMin = minimum [x0, x1]
      xMax = maximum [x0, x1]
    case (lowerBound, upperBound) of
      (Nothing, Nothing) -> Interval <$> elements [Nothing, Just xMin] <*> elements [Nothing, Just xMax]
      (Nothing, Just _ ) -> Interval <$> elements [Nothing, Just xMin] <*> return   (         Just xMax)
      (Just _ , Nothing) -> Interval <$> return   (         Just xMin) <*> elements [Nothing, Just xMax]
      (Just _ , Just _ ) -> Interval <$> return   (         Just xMin) <*> return   (         Just xMax)
arbitrarySubdomain Set{..} =
  case options of
    xs -> Set <$> sublistOf xs
  

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
      <*> frequency [(1, return 1), (9, arbitrary `suchThat` (/= 0))]
