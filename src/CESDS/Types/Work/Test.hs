{-# OPTIONS_GHC -fno-warn-orphans #-}


module CESDS.Types.Work.Test (
) where


import CESDS.Types.Test ()
import CESDS.Types.Work (Submission(..), SubmissionResult(..), WorkStatus(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof)


instance Arbitrary Submission where
  arbitrary = Submission <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


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
