{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Server.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Server (APIError(..), Server(..), Status(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (frequency, resize)


instance Arbitrary Server where
  arbitrary = Server <$> arbitrary <*> arbitrary <*> resize 4 arbitrary <*> arbitrary


instance Arbitrary Status where
  arbitrary =
    frequency
      [
        (7, return Okay              )
      , (1, return Broken            )
      , (1, return OnFire            )
      , (1, OtherStatus <$> arbitrary)
      ]  


instance Arbitrary APIError where
  arbitrary = APIError <$> arbitrary
