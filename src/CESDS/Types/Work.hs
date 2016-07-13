{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module CESDS.Types.Work (
  WorkIdentifier
, Duration
, Priority
, Submission(..)
, validateSubmission
, SubmissionResult(..)
, Work(..)
, WorkList(..)
, makeWorkList
, validateWorkList
, maybeRecordIdentifier
, hasStatus
, isSuccess
) where


import CESDS.Types (Generation, Identifier, Val, object', valAsString)
import CESDS.Types.Record (RecordIdentifier)
import CESDS.Types.Variable (VariableIdentifier, canHaveVal, hasVariable)
import Control.Arrow (second)
import Control.Monad.Except (MonadError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(parseJSON), Parser, ToJSON(toJSON), Value, (.:), (.:?), (.=), object, withObject)
import Data.Function (on)
import Data.HashMap.Strict (toList)
import Data.List (sort, sortBy)
import Data.List.Util (disjoint, hasSubset, noDuplicates)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Variable as Variable (Variable(..))


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


validateSubmission :: (IsString e, MonadError e m) => Model.Model -> [String] -> Submission -> m ()
validateSubmission Model.Model{..} recordKeys Submission{..} =
  do
    let
      modelVariables = map Variable.identifier variables
      inputVariables = map Variable.identifier $ filter Variable.isInput variables
      explicitVariables' = sort $ map fst explicitVariables
      randomVariables' = sort randomVariables
    assert "cannot set value of output variable" $ null inputVariables || inputVariables `hasSubset` (explicitVariables' ++ randomVariables')
    assert "duplicate explicit variables" $ noDuplicates explicitVariables'
    assert "invalid explicit variable" $ modelVariables `hasSubset` explicitVariables'
    assert "duplicate random variables" $ noDuplicates randomVariables'
    assert "invalid random variable" $ modelVariables `hasSubset` randomVariables'
    assert "explicit and random variables overlap" $ explicitVariables' `disjoint` randomVariables'
    assert "timeout cannot be negative" $ maybe True (> 0) timeout
    sequence_
      [
        do
          variable <-variables `hasVariable` variableIdentifier
          Variable.domain variable `canHaveVal` value
          assert "primary key violation"  $variableIdentifier `notElem` explicitVariables' || valAsString value `notElem` recordKeys
      |
        (variableIdentifier, value) <- explicitVariables
      ]
    sequence_
      [
        variables `hasVariable` variableIdentifier
      |
        variableIdentifier <- randomVariables'
      ]


data SubmissionResult =
    Submitted
    {
      identifier          :: WorkIdentifier
    , generation          :: Generation
    , estimatedCompletion :: Maybe Duration
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON SubmissionResult where
  parseJSON =
    withObject "WORK_SUBMISSION_RESULT" $ \o ->
      do
        identifier          <- o .:  "work_id"
        generation          <- o .:  "generation"
        estimatedCompletion <- o .:? "estimated_completion"
        return Submitted{..}

instance ToJSON SubmissionResult where
  toJSON Submitted{..} =
    object'
      [
        "work_id"              .= identifier
      , "generation"           .= generation
      , "estimated_completion" .= estimatedCompletion
      ]


data Work =
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

instance FromJSON Work where
  parseJSON = 
    withObject "WORK_ITEM" $ \o ->
      do
        status <- o .: "status"
        case status of
          "pending"  -> Pending <$> o .: "work_id"
          "running"  -> Running <$> o .: "work_id"
          "success"  -> Success <$> o .: "work_id" <*> o .: "result_id"
          "failed"   -> Failure <$> o .: "work_id" <*> o .: "additional"
          _          -> fail $ "invalid WORK_STATUS \"" ++ status ++ "\""
        
instance ToJSON Work where
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


data WorkList =
  WorkList
  {
    count  :: Int
  , statuses :: [Work]
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON WorkList where
  parseJSON =
    withObject "WORK_ITEM_LIST" $ \o ->
      do
        count    <- o .: "count"
        statuses <- o .: "status"
        return WorkList{..}

instance ToJSON WorkList where
  toJSON WorkList{..} = object' ["count" .= count, "status" .= statuses]


makeWorkList :: [Work] -> WorkList
makeWorkList statuses =
  let
    count = length statuses
  in
    WorkList{..}


validateWorkList :: (IsString e, MonadError e m) => WorkList -> m ()
validateWorkList WorkList{..} =
  do
    assert "duplicate work identifiers" $ noDuplicates $ map workIdentifier statuses
    assert "duplicate record identifiers in work" $ noDuplicates . catMaybes $ map maybeRecordIdentifier statuses


maybeRecordIdentifier :: Work -> Maybe RecordIdentifier
maybeRecordIdentifier Success{..} = Just recordIdentifier
maybeRecordIdentifier _           = Nothing


hasStatus :: Text -> Work -> Bool
hasStatus "pending" Pending{..} = True
hasStatus "running" Running{..} = True
hasStatus "success" Success{..} = True
hasStatus "failed"  Failure{..} = True
hasStatus _         _           = False


isSuccess :: Work -> Bool
isSuccess Success{} = True
isSuccess _         = False
