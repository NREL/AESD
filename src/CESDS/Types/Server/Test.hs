{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Server.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Server (Server(..), Status(..))
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (frequency, resize, suchThat)


instance Arbitrary Server where
  arbitrary =
    Server
      <$> arbitrary `suchThat` (/= "")
      <*> frequency [(49, return "record_server"), (1, arbitrary)]
      <*> frequency [(49, return 0              ), (1, arbitrary)]
      <*> (nub <$> resize 4 arbitrary)
      <*> arbitrary


instance Arbitrary Status where
  arbitrary =
    frequency
      [
        (9, Okay    <$> arbitrary)
      , (1, Failure <$> arbitrary)
      ]  
