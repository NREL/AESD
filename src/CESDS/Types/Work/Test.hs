{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Work.Test (
  arbitrarySubmission
) where


import CESDS.Types.Record (Record(..))
import CESDS.Types.Record.Test (arbitraryRecord)
import CESDS.Types.Test (arbitraryPositive)
import CESDS.Types.Variable (Variable)
import CESDS.Types.Variable.Test ()
import CESDS.Types.Work (Submission(..), SubmissionResult(..), Work(..))
import Data.List ((\\))
import Data.List.Util (nubOn)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, oneof, resize, sublistOf)

import qualified CESDS.Types.Variable as Variable (Variable(..))


arbitrarySubmission :: [Variable] -> Gen Submission
arbitrarySubmission variables = -- FIXME: This can generate submission with a primary key violation.
  do
    let
      inputVariables = map Variable.identifier $ filter Variable.isInput variables
    randomVariables <- sublistOf inputVariables
    explicitVariables' <- sublistOf (inputVariables \\ randomVariables)
    record <- arbitraryRecord variables
    let
      explicitVariables = filter ((`elem` explicitVariables') . fst) $ recordValues record
    timeout <- arbitraryPositive
    priority <- arbitrary
    return Submission{..}

      
instance Arbitrary Submission where
  arbitrary = arbitrarySubmission =<< (nubOn Variable.identifier <$> resize 4 arbitrary)


instance Arbitrary SubmissionResult where
  arbitrary =
    Submitted
      <$> arbitrary
      <*> arbitrary
      <*> arbitraryPositive


instance Arbitrary Work where
  arbitrary =
    oneof
      [
        Pending <$> arbitrary
      , Running <$> arbitrary
      , Success <$> arbitrary <*> arbitrary
      , Failure <$> arbitrary <*> arbitrary
      ]
