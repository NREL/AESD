{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Filter.Test (
  arbitraryFilter
, arbitraryExpression
) where


import CESDS.Types.Test (arbitraryPositive, arbitraryVal)
import CESDS.Types.Filter (Filter(..), FilterIdentifier, SelectionExpression(..))
import CESDS.Types.Variable (Variable(..))
import CESDS.Types.Variable.Test (arbitrarySubdomain)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, frequency, listOf1, oneof, suchThat)


arbitraryFilter :: [Maybe FilterIdentifier] -> [Variable] -> Gen Filter
arbitraryFilter veto variables =
    Filter
      <$> arbitrary `suchThat` (`notElem` veto)
      <*> arbitrary
      <*> arbitraryPositive
      <*> arbitrary
      <*> arbitrary
      <*> oneof [return Nothing, Just <$> arbitraryExpression variables]


instance Arbitrary Filter where
  arbitrary = arbitraryFilter [] =<< listOf1 arbitrary


arbitraryExpression :: [Variable] -> Gen SelectionExpression
arbitraryExpression variables =
    frequency
      [
        (1, NotSelection       <$> arbitraryExpression variables                                          )
      , (1, UnionSelection     <$> arbitraryExpression variables <*> arbitraryExpression variables        )
      , (1, IntersectSelection <$> arbitraryExpression variables <*> arbitraryExpression variables        )
      , (2, oneof [ValueSelection  identifier <$> arbitraryVal       domain   | Variable{..} <- variables])
      , (5, oneof [DomainSelection identifier <$> arbitrarySubdomain domain   | Variable{..} <- variables])
      ]


instance Arbitrary SelectionExpression where
  arbitrary = arbitraryExpression =<< listOf1 arbitrary
