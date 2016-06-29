{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Command.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Command (Command(..), Result(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof, resize)


instance Arbitrary Command where
  arbitrary =
    oneof
      [
        Restart        <$> resize 4 arbitrary
      , Clear          <$> resize 4 arbitrary
      , StrategyRandom <$> resize 4 arbitrary
      , StrategyFIFO   <$> resize 4 arbitrary
      , StrategyFILO   <$> resize 4 arbitrary
      ]


instance Arbitrary Result where
  arbitrary =
    oneof
      [
        return Success
      , Error <$> arbitrary <*> arbitrary
      ]
