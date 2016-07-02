{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Work.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Work (Submission(..), SubmissionResult(..), WorkStatus(..))
import Data.Function (on)
import Data.List (nub, sortBy)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof, resize)


instance Arbitrary Submission where
  arbitrary =
    Submission
      <$> (nub . sortBy (compare `on` fst) <$> resize 4 arbitrary)
      <*> (nub <$> resize 4 arbitrary)
      <*> arbitrary
      <*> arbitrary


instance Arbitrary SubmissionResult where
  arbitrary =
    oneof
      [
        Submitted <$> arbitrary <*> arbitrary <*> arbitrary
      , SubmissionError <$> arbitrary
      ]


instance Arbitrary WorkStatus where
  arbitrary =
    oneof
      [
        Pending <$> arbitrary
      , Running <$> arbitrary
      , Success <$> arbitrary <*> arbitrary
      , Failure <$> arbitrary <*> arbitrary
      ]
