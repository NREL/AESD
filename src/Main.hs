{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Server (ServerM, Service(..), gets, modifys, runService)
import CESDS.Types (Generation)
import CESDS.Types.Model (Model, ModelIdentifier)
import CESDS.Types.Model.Test (arbitraryModel)
import CESDS.Types.Record (Record, RecordIdentifier)
import CESDS.Types.Record.Test (arbitraryRecord)
import CESDS.Types.Server (Server)
import CESDS.Types.Server.Test ()
import CESDS.Types.Variable (Variable, VariableIdentifier)
import CESDS.Types.Variable.Test (arbitraryVariable)
import CESDS.Types.Work (WorkStatus)
import CESDS.Types.Work.Test ()
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, frequency, generate, listOf)

import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Server as Server (Server(..))


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
              variables <-
                sequence
                  [
                    (variable, ) <$> arbitraryVariable variable
                  |
                    variable <- Model.variables model
                  ]
              works <- listOf $ arbitraryWorkState generation
              records <- listOf $ do
                                    k <- arbitrary
                                    v <- arbitraryRecord $ map snd variables
                                    return ((Model.generation model, k), v)
              return (identifier, ModelState{..})
          |
            identifier <- Server.models server
          ]
      return ServerState{..}


data ModelState =
  ModelState
  {
    model     :: Model
  , variables :: [(VariableIdentifier, Variable)]
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


data RecordState =
  RecordState
  {
    recordGeneration :: Generation
  , recordIdentifier :: RecordIdentifier
  , record           :: Record
  }
    deriving (Eq, Read, Show)


arbitraryRecordState :: Generation -> RecordIdentifier -> [Variable] -> Gen WorkState
arbitraryRecordState recordGeneration recordIdentifier variables =
  do
    record <- arbitraryRecord variables
    return RecordState{..}


initialize :: IO ServerState
initialize = generate arbitrary


service :: Service ServerState
service =
  let
    getServer = gets server
    postServer (Command.Restart _) =
      randomFailure
        $ do
          new <- liftIO initialize
          modifys $ const new
          return Command.Success
    postServer _ = notImplemented
    getModel identifier = gets (fmap model . lookup identifier . models)
    postModel (Command.Restart _) identifier =
      randomFailure
        $ maybe
          (return $ Command.Error "model not found" Nothing)
          (const $ return Command.Success)
        =<< gets (lookup identifier . models)
    postModel _ _ = notImplemented
    getWorks (f, t, s, w) identifier =
      let
      in
        gets
          $ fromMaybe []
          . fmap (map snd . works)
          . lookup identifier
          . models
    getRecords (f, t, k, r) identifier =
      let
      in
        gets
          $ fromMaybe []
          . fmap (map snd . records)
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
