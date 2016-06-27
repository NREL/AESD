{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Command.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Command (Command(..), Result(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof)


instance Arbitrary Command where
  arbitrary =
    oneof
      [
        return RestartServer
      , return ClearServer
      , RestartModel   <$> arbitrary
      , ClearModel     <$> arbitrary
      , StrategyRandom <$> arbitrary
      , StrategyFIFO   <$> arbitrary
      , StrategyFILO   <$> arbitrary
      ]


instance Arbitrary Result where
  arbitrary =
    oneof
      [
        return Success
      , Error <$> arbitrary <*> arbitrary
      ]
