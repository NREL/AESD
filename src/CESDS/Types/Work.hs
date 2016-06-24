{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Work (
  WorkIdentifier
, Duration
, Priority
, Submission(..)
, SubmissionResult(..)
, Status(..)
) where


import CESDS.Types (Generation, Identifier)
import CESDS.Types.Result (ResultIdentifier)
import CESDS.Types.Variable (VariableIdentifier)
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)


type WorkIdentifier = Identifier


type Duration = Double


type Priority = Int


data Submission =
  Submission
  {
    explicitVariables :: [(VariableIdentifier, Scientific)]
  , randomVariables   :: [VariableIdentifier]
  , timeout           :: Maybe Duration
  , priority          :: Maybe Priority
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


data SubmissionResult =
    Submitted
    {
      workID              :: WorkIdentifier
    , generation'         :: Generation
    , estimatedCompletion :: Maybe Duration
    }
  | Error
    {
      error :: Text
    }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


data Status =
    Pending
    {
      workID' :: WorkIdentifier
    }
  | Running
    {
      workID' :: WorkIdentifier
    }
  | Success
    {
      workID'  :: WorkIdentifier
    , resultID :: ResultIdentifier
    }
  | Failure
    {
      workID' :: WorkIdentifier
    , additional :: Text
    }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
