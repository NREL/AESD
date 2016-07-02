{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module CESDS.Types.Work (
  WorkIdentifier
, Duration
, Priority
, Submission(..)
, SubmissionResult(..)
, WorkStatus(..)
, maybeRecordIdentifier
, hasStatus
, isSuccess
) where


import CESDS.Types (Generation, Identifier, Val, object')
import CESDS.Types.Record (RecordIdentifier)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Data.Aeson.Types (FromJSON(parseJSON), Parser, ToJSON(toJSON), Value, (.:), (.:?), (.=), object, withObject)
import Data.Function (on)
import Data.HashMap.Strict (toList)
import Data.List (sort, sortBy)
import Data.Text (Text)
import GHC.Generics (Generic)


type WorkIdentifier = Identifier


type Duration = Double


type Priority = Int


data Submission =
  Submission
  {
    explicitVariables :: [(VariableIdentifier, Val)]
  , randomVariables   :: [VariableIdentifier]
  , timeout           :: Maybe Duration
  , priority          :: Maybe Priority
  }
    deriving (Generic, Read, Show)

instance Eq Submission where
  x == y =
      sortBy (compare `on` fst) (explicitVariables x) == sortBy (compare `on` fst) (explicitVariables y)
      && sort (randomVariables x) == sort (randomVariables y)
      && timeout x == timeout y
      && priority x == priority y
instance FromJSON Submission where
  parseJSON =
    withObject "WORK_SUBMISSION" $ \o ->
      do
        let
          parseExplicit :: Value -> Parser [(Text, Val)]
          parseExplicit =
            withObject "WORK_SUBMISSION explicit" $ \o'' ->
              do
                let
                  o' = toList o''
                sequence
                  [
                    (k, ) <$> parseJSON v
                  |
                    (k, v) <- o'
                  ]
        explicitVariables <- (o .: "explicit") >>= parseExplicit
        randomVariables   <- o .:  "random"
        timeout           <- o .:? "timeout"
        priority          <- o .:? "priority"
        return Submission{..}

instance ToJSON Submission where
  toJSON Submission{..} =
    object'
      [
        "explicit" .= object (map (second toJSON) explicitVariables)
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


maybeRecordIdentifier :: WorkStatus -> Maybe RecordIdentifier
maybeRecordIdentifier Success{..} = Just recordIdentifier
maybeRecordIdentifier _           = Nothing


hasStatus :: Text -> WorkStatus -> Bool
hasStatus "pending" Pending{..} = True
hasStatus "running" Running{..} = True
hasStatus "success" Success{..} = True
hasStatus "failed"  Failure{..} = True
hasStatus _         _           = False


isSuccess :: WorkStatus -> Bool
isSuccess Success{} = True
isSuccess _         = False
