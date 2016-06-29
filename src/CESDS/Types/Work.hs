{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Work (
  WorkIdentifier
, Duration
, Priority
, Submission(..)
, SubmissionResult(..)
, WorkStatus(..)
) where


import CESDS.Types (Generation, Identifier, object')
import CESDS.Types.Record (RecordIdentifier)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Value, (.:), (.:?), (.=), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)


type WorkIdentifier = Identifier


type Duration = Double


type Priority = Int


data Submission =
  Submission
  {
    explicitVariables :: [(VariableIdentifier, Value)]
  , randomVariables   :: [VariableIdentifier]
  , timeout           :: Maybe Duration
  , priority          :: Maybe Priority
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Submission where
  parseJSON =
    withObject "WORK_SUBMISSION" $ \o ->
      do
        explicitVariables <- o .:  "explicit"
        randomVariables   <- o .:  "random"
        timeout           <- o .:? "timeout"
        priority          <- o .:? "priority"
        return Submission{..}

instance ToJSON Submission where
  toJSON Submission{..} =
    object'
      [
        "explicit" .= explicitVariables
      , "random"   .= randomVariables
      , "timeout"  .= timeout
      , "priority" .= priority
      ]


data SubmissionResult =
    Submitted
    {
      identifier          :: WorkIdentifier
    , generation          :: Generation
    , estimatedCompletion :: Maybe Duration
    }
  | SubmissionError
    {
      message :: Text
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON SubmissionResult where
  parseJSON =
    withObject "WORK_SUBMISSION_RESULT" $ \o ->
      parseSubmitted o <|> parseError o
    where
      parseSubmitted o =
        do
          identifier          <- o .:  "work_id"
          generation          <- o .:  "generation"
          estimatedCompletion <- o .:? "estimated_completion"
          return Submitted{..}
      parseError o =
        do
          message <- o .: "error"
          return SubmissionError{..}

instance ToJSON SubmissionResult where
  toJSON Submitted{..} =
    object'
      [
        "work_id"              .= identifier
      , "generation"           .= generation
      , "estimated_completion" .= estimatedCompletion
      ]
  toJSON SubmissionError{..} =
    object'
      [
        "error" .= message
      ]


data WorkStatus =
    Pending
    {
      workIdentifier :: WorkIdentifier
    }
  | Running
    {
      workIdentifier :: WorkIdentifier
    }
  | Success
    {
      workIdentifier   :: WorkIdentifier
    , recordIdentifier :: RecordIdentifier
    }
  | Failure
    {
      workIdentifier :: WorkIdentifier
    , reason         :: Text
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON WorkStatus where
  parseJSON = 
    withObject "WORK_STATUS" $ \o ->
      do
        status <- o .: "status"
        case status of
          "pending"  -> Pending <$> o .: "work_id"
          "running"  -> Running <$> o .: "work_id"
          "success"  -> Success <$> o .: "work_id" <*> o .: "result_id"
          "failed"   -> Failure <$> o .: "work_id" <*> o .: "additional"
          _          -> fail $ "invalid WORK_STATUS \"" ++ status ++ "\""
        
instance ToJSON WorkStatus where
  toJSON Pending{..} =
    object'
      [
        "status"  .= ("pending" :: String)
      , "work_id" .= workIdentifier
      ]
  toJSON Running{..} =
    object'
      [
        "status"  .= ("running" :: String)
      , "work_id" .= workIdentifier
      ]
  toJSON Success{..} =
    object'
      [
        "status"    .= ("success" :: String)
      , "work_id"   .= workIdentifier
      , "result_id" .= recordIdentifier       
      ]
  toJSON Failure{..} =
    object'
      [
        "status"     .= ("failed" :: String)
      , "work_id"    .= workIdentifier
      , "additional" .= reason    
      ]
