{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Server (RecordFilter(..), ServerM, Service(..), WorkFilter(..), gets, modifys, runService)
import CESDS.Types (Generation, valAsString)
import CESDS.Types.Model (Model, ModelIdentifier)
import CESDS.Types.Model.Test (arbitraryModel)
import CESDS.Types.Record (Record, RecordIdentifier)
import CESDS.Types.Record.Test (arbitraryRecord)
import CESDS.Types.Server (Server)
import CESDS.Types.Server.Test ()
import CESDS.Types.Variable (Variable, VariableIdentifier)
import CESDS.Types.Variable.Test ()
import CESDS.Types.Work (WorkStatus, maybeRecordIdentifier)
import CESDS.Types.Work.Test ()
import Control.Monad.Reader (liftIO)
import Data.List (find)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (pack)
import Debug.Trace (trace)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, frequency, generate, listOf)

import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Record as Record (Record(..))
import qualified CESDS.Types.Server as Server (Server(..))
import qualified CESDS.Types.Work as Work (WorkStatus(..), hasStatus)


data ServerState =
  ServerState
  {
    server :: Server
  , models :: [(ModelIdentifier, ModelState)]
  }
    deriving (Eq, Read, Show)

instance Arbitrary ServerState where
  arbitrary =
    do
      server <- arbitrary
      models <-
        sequence
          [
            do
              model <- arbitraryModel identifier
              let generation = Model.generation model
              works <- listOf $ arbitraryWorkState generation
              records <-
                sequence
                  [
                    arbitraryRecordState
                      generation
                      (fromJust $ maybeRecordIdentifier workStatus)
                      (Model.variables model)
                      (Model.primaryKey model)
                  |
                    WorkState{..} <- works
                  , isJust $ maybeRecordIdentifier workStatus
                  ]
              return (identifier, ModelState{..})
          |
            identifier <- Server.models server
          ]
      return ServerState{..}


data ModelState =
  ModelState
  {
    model     :: Model
  , works     :: [WorkState]
  , records   :: [RecordState]
  }
    deriving (Eq, Read, Show)


data WorkState =
  WorkState
  {
    workGeneration :: Generation
  , workStatus     :: WorkStatus
  }
    deriving (Eq, Read, Show)


arbitraryWorkState :: Generation -> Gen WorkState
arbitraryWorkState workGeneration =
  do
    workStatus <- arbitrary
    return WorkState{..}


maybeCompare :: Maybe a -> (Maybe a -> Maybe a -> Bool) -> a -> Bool
maybeCompare x f y = isNothing x || x `f` Just y


filterWorkState :: WorkFilter -> [WorkState] -> [WorkState]
filterWorkState WorkFilter{..} =
  filter f
    where
      f WorkState{..} =
           maybeCompare wfFrom (<=) workGeneration
        && maybeCompare wfTo   (>=) workGeneration
        && maybeCompare wfWork (==) (Work.workIdentifier workStatus)
        && maybe True (flip Work.hasStatus workStatus . pack) wfStatus


data RecordState =
  RecordState
  {
    recordGeneration :: Generation
  , recordIdentifier :: RecordIdentifier
  , recordKey        :: String
  , record           :: Record
  }
    deriving (Eq, Read, Show)


filterRecordState :: RecordFilter -> [RecordState] -> [RecordState]
filterRecordState RecordFilter{..} =
  filter f
    where
      f RecordState{..} =
           maybeCompare rfFrom   (<=) recordGeneration
        && maybeCompare rfTo     (>=) recordGeneration
        && maybeCompare rfRecord (==) recordIdentifier 
        && maybeCompare rfKey    (==) recordKey


arbitraryRecordState :: Generation -> RecordIdentifier -> [Variable] -> VariableIdentifier -> Gen RecordState
arbitraryRecordState recordGeneration recordIdentifier variables key =
  do
    record <- arbitraryRecord variables
    let
       recordKey = (\x -> trace (show x) x) . valAsString . snd . fromJust . find ((== key) . fst) $ Record.unRecord record
    return RecordState{..}


initialize :: IO ServerState
initialize = generate arbitrary


service :: Service ServerState
service =
  let
    getServer =
      gets server
    postServer (Command.Restart _) =
      randomFailure
        $ do
          new <- liftIO initialize
          modifys $ const new
          return Command.Success
    postServer _ =
      notImplemented
    getModel identifier =
      gets
        $ fmap model
        . lookup identifier
        . models
    postModel (Command.Restart _) identifier =
      randomFailure
        $ maybe
          (return $ Command.Error "model not found" Nothing)
          (const $ return Command.Success)
        =<< gets (lookup identifier . models)
    postModel _ _ =
      notImplemented
    getWorks workFilter identifier =
      gets
        $ maybe
          []
          (map workStatus . filterWorkState workFilter . works)
        . lookup identifier
        . models
    getRecords recordFilter identifier =
      gets
        $ maybe
          []
          (map record . filterRecordState recordFilter . records)
        . lookup identifier
        . models
  in
    Service{..}


main :: IO ()
main = runService 8090 service =<< initialize


notImplemented :: ServerM ServerState Command.Result
notImplemented = return $ Command.Error "not implemented" Nothing


randomFailure :: ServerM ServerState Command.Result -> ServerM ServerState Command.Result
randomFailure x =
  do
    outcome <- randomOutcome 9
    if outcome
      then x
      else return $ Command.Error "failed" $ Just "random failure for testing"


randomOutcome :: Int -> ServerM ServerState Bool
randomOutcome success =
  liftIO
    . generate
    $ frequency [(success, return True), (1, return False)]


{-

POST /server/MODEL_ID/work WORK_SUBMISSION WORK_SUBMISSION_RESULT

GET /server/MODEL/bookmark_metas TAG_ID multiple BOOKMARK_META

GET /server/MODEL/bookmarks bookmark_id=BOOKMARK_ID BOOKMARK

POST /server/MODEL_ID/bookmarks BOOKMARK w/o ID BOOKMARK_META

GET /server/MODEL_ID/filter_metas TAG_ID=TEXT multiple optional FILTER_META

GET /server/MODEL_ID/filters filter_id=FILTER_ID optional FILTER

POST /server/MODEL_ID/filters FILTER w/o ID FILTER_META

-}
