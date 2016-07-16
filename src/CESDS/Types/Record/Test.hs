{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Record.Test (
  arbitraryRecord
) where


import CESDS.Types.Test (arbitraryVal)
import CESDS.Types.Variable (Variable(..))
import CESDS.Types.Variable.Test ()
import CESDS.Types.Record (Record(..))
import Data.List.Util (nubOn)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, resize)


arbitraryRecord :: [Variable] -> Gen Record 
arbitraryRecord variables =
  do
    recordIdentifier <- arbitrary
    recordValues <-
       sequence
         [
           (identifier, ) <$> arbitraryVal domain
         |
           Variable{..} <- variables
         ]
    return Record{..}


instance Arbitrary Record where
  arbitrary = arbitraryRecord =<< nubOn identifier <$> resize 4 (arbitrary :: Gen [Variable])
