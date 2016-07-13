{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Record.Test (
  arbitraryRecord
, arbitraryRecordList
) where


import CESDS.Types.Test (arbitraryVal)
import CESDS.Types.Variable (Variable(..))
import CESDS.Types.Variable.Test ()
import CESDS.Types.Record (Record(..), RecordList(..), makeRecordList)
import Data.List.Util (nubOn)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, listOf, resize)


arbitraryRecord :: [Variable] -> Gen Record 
arbitraryRecord variables =
  Record <$> sequence
    [
      (identifier, ) <$> arbitraryVal domain
    |
      Variable{..} <- variables
    ]


instance Arbitrary Record where
  arbitrary = Record . nubOn fst <$> resize 4 arbitrary


arbitraryRecordList :: [Variable] -> Gen RecordList
arbitraryRecordList variables =
  makeRecordList <$> resize 4 (listOf $ arbitraryRecord variables)


instance Arbitrary RecordList where
  arbitrary =
    do
      variables <- nubOn identifier <$> resize 4 arbitrary
      arbitraryRecordList variables
