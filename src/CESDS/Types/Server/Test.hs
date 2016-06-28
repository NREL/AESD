{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Server.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Server (Server(..), Status(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof, resize)


instance Arbitrary Server where
  arbitrary = Server <$> arbitrary <*> arbitrary <*> resize 4 arbitrary <*> arbitrary


instance Arbitrary Status where
  arbitrary =
    oneof
      [
        return Okay
      , return Broken
      , return OnFire
      , OtherStatus <$> arbitrary
      ]  
