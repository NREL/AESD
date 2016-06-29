{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Filter.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Filter (Filter(..), SelectionExpression(..))
import CESDS.Types.Variable (Domain(..))
import CESDS.Types.Variable.Test ()
import Data.Maybe (fromMaybe)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (frequency)


instance Arbitrary Filter where
  arbitrary =
    Filter
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary


instance Arbitrary SelectionExpression where
  arbitrary =
    frequency
      [
        (1, NotSelection       <$> arbitrary                          )
      , (1, UnionSelection     <$> arbitrary <*>            arbitrary )
      , (1, IntersectSelection <$> arbitrary <*>            arbitrary )
      , (2, ValueSelection     <$> arbitrary <*>            arbitrary )
      , (5, DomainSelection    <$> arbitrary <*> (patch <$> arbitrary))
      ]
    where
      patch :: Domain -> Domain
      patch i@Interval{} = i
      patch   Set{..}    = Set {options = Just $ fromMaybe [] options}
