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
  arbitrary = Filter <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary SelectionExpression where
  arbitrary =
    frequency
      [
        (0, NotSelection       <$> arbitrary                          )
      , (0, UnionSelection     <$> arbitrary <*>            arbitrary )
      , (0, IntersectSelection <$> arbitrary <*>            arbitrary )
      , (5, ValueSelection     <$> arbitrary <*>            arbitrary )
      , (1, DomainSelection    <$> arbitrary <*> (patch <$> arbitrary))
      ]
    where
      patch :: Domain -> Domain
      patch   Set{..}    = Set {options = Just $ fromMaybe [] options}
      patch i@Interval{} = i
