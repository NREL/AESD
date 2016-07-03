{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Server (RecordFilter(..), ServerM, Service(..), WorkFilter(..), gets, modifys, modifysIO, runService, sets)
import CESDS.Types (Generation, isContinuous, isDiscrete, valAsString)
import CESDS.Types.Model (Model, ModelIdentifier)
import CESDS.Types.Model.Test (arbitraryModel)
import CESDS.Types.Record (Record, RecordIdentifier)
import CESDS.Types.Record.Test (arbitraryRecord)
import CESDS.Types.Server (Server)
import CESDS.Types.Server.Test ()
import CESDS.Types.Variable (isInterval, isSet)
import CESDS.Types.Variable.Test ()
import CESDS.Types.Work (WorkStatus, maybeRecordIdentifier)
import CESDS.Types.Work.Test ()
import Control.Monad (foldM, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Function (on)
import Data.List ((\\), find, intersect, nub, nubBy, sort)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Text (pack)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, frequency, generate)

import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Record as Record (Record(..))
import qualified CESDS.Types.Server as Server (Server(..))
import qualified CESDS.Types.Variable as Variable (Variable(..))
import qualified CESDS.Types.Work as Work (Submission(..), SubmissionResult(..), WorkStatus(..), hasStatus, isSuccess)


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
            (modelIdentifier, ) <$> arbitraryModelState modelIdentifier
          |
            modelIdentifier <- Server.models server
          ]
      return ServerState{..}


data ModelState =
  ModelState
  {
    model     :: Model
  , works     :: [WorkState]
  }
    deriving (Eq, Read, Show)


ageModels :: ServerState -> IO ServerState
ageModels serverState@ServerState{..} =
  do
    models' <-
      sequence
        [
          (modelIdentifier, ) <$> ageModel model
        |
          (modelIdentifier, model) <- models
        ]
    return serverState {models = models'}


ageModel :: ModelState -> IO ModelState
ageModel modelState@ModelState{..} =
  do
    works' <-
      sequence
        [
          do
            r <- generate $ choose (0, 1) :: IO Double
            return $
              workState
              {
                workStatus = case workStatus of
                  Work.Pending{..} -> if r < 0.25 then Work.Running workIdentifier else workStatus
                  Work.Running{..} -> if r < 0.50
                                        then if r < 0.10
                                               then Work.Failure workIdentifier "random failure"
                                               else Work.Success workIdentifier recordIdentifier
                                        else workStatus
                  _                -> workStatus
              }
        |
          workState@WorkState{..} <- works
        ]
    return modelState {works = works'}


arbitraryModelState :: ModelIdentifier -> Gen ModelState
arbitraryModelState modelIdentifier =
  do
    modelState <- ModelState <$> arbitraryModel modelIdentifier <*> return []
    let
      submission = Work.Submission [] (map Variable.identifier . Model.variables $ model modelState) Nothing Nothing
    n <- choose (0, 4) :: Gen Int
    foldM (const . addArbitraryWork submission) modelState [1..n]


addArbitraryWork :: Work.Submission -> ModelState -> Gen ModelState
addArbitraryWork submission@Work.Submission{..} modelState@ModelState{..} =
  do
    let
      Model.Model{..} = model
      workGeneration = generation
    workStatus <- arbitrary
    recordIdentifier <- maybe arbitrary return $ maybeRecordIdentifier workStatus
    record' <- arbitraryRecord variables
    let
      record =
        Record.Record
          [
            (k , fromMaybe v $ k `lookup` explicitVariables)
          |
            (k, v) <- Record.unRecord record'
          ]
      recordKey = valAsString . snd . fromJust . find ((== primaryKey) . fst) $ Record.unRecord record
    let
      w = WorkState{..}
      fWorkIdentifier = Work.workIdentifier . Main.workStatus
      fRecordIdentifier = maybeRecordIdentifier . Main.workStatus
    if fWorkIdentifier w `elem` map (Work.workIdentifier . Main.workStatus) works
        || fRecordIdentifier w `elem` map fRecordIdentifier works
        || recordKey `elem` map Main.recordKey works
      then addArbitraryWork submission modelState
      else return modelState {works = w : works}


data WorkState =
  WorkState
  {
    workGeneration   :: Generation
  , workStatus       :: WorkStatus
  , recordIdentifier :: RecordIdentifier
  , recordKey        :: String
  , record           :: Record
  }
    deriving (Eq, Read, Show)


submitWork :: ModelState -> Work.Submission -> ServerM s (ModelState, Work.SubmissionResult)
submitWork ms@ModelState{..} su =
  do
    let
      generation' = Model.generation model + 1
      modelVariables = map Variable.identifier $ Model.variables model
      explicitVariables' = sort . map fst $ Work.explicitVariables su
      randomVariables' = sort $ Work.randomVariables su
      noDuplicates x = length x == length (nub x)
      x `hasSubset` y = null $ y \\ x
      x `doesntOverlap` y = null $ x `intersect` y
      key = Model.primaryKey model
      recordKeys = map recordKey works
    unless (noDuplicates explicitVariables')
      $ throwError "duplicate explicit variables"
    unless (modelVariables `hasSubset` explicitVariables')
      $ throwError "explicit variable not found among model variables"
    unless (noDuplicates randomVariables')
      $ throwError "duplicate random variables"
    unless (modelVariables `hasSubset` randomVariables')
      $ throwError "random variable not found among model variables"
    unless (explicitVariables' `doesntOverlap` randomVariables')
      $ throwError "explicit and random variables overlap"
    unless (map Variable.identifier (filter (isInterval . Variable.domain) $ Model.variables model) `hasSubset` map fst (filter (isContinuous . snd) $ Work.explicitVariables su))
      $ throwError "continuous value specified for discrete variable"
    unless (map Variable.identifier (filter (isSet . Variable.domain) $ Model.variables model) `hasSubset` map fst (filter (isDiscrete . snd) $ Work.explicitVariables su))
      $ throwError "discrete value specified for continuous variable"
    unless (key `notElem` explicitVariables' || fmap valAsString (lookup key $ Work.explicitVariables su) `notElem` map Just recordKeys)
      $ throwError "primary key violation"
    ms' <- liftIO $ generate $ addArbitraryWork su $ ms {model = model {Model.generation = generation'}}
    return
      (
        ms'
      , Work.Submitted
        {
          identifier          = Work.workIdentifier . workStatus $ head $ Main.works ms'
        , generation          = generation'
        , estimatedCompletion = Nothing
        }
      )
    

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


filterRecordState :: RecordFilter -> [WorkState] -> [WorkState]
filterRecordState RecordFilter{..} =
  filter f
    where
      f WorkState{..} =
           Work.isSuccess workStatus
        && maybeCompare rfFrom   (<=) workGeneration
        && maybeCompare rfTo     (>=) workGeneration
        && maybeCompare rfRecord (==) recordIdentifier 
        && maybeCompare rfKey    (==) recordKey


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
          sets new
          return Command.Success
    postServer _ =
      notImplemented
    getModel identifier =
      do
        modelState' <- gets $ lookup identifier . models
        maybeModelNotFound
          (return . model)
          modelState'
    postModel (Command.Restart _) identifier =
      randomFailure
        $ do
          model' <- gets $ lookup identifier . models
          maybe
            (return $ Command.Error "model not found" Nothing)
            (const $ return Command.Success)
            model'
    postModel _ _ =
      notImplemented
    getWorks workFilter identifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup identifier . models
        maybeModelNotFound
          (return . map workStatus . filterWorkState workFilter . works)
          modelState'
    postWork submission identifier =
      do
        modelState' <- gets $ lookup identifier . models
        maybe
          (return $ Work.SubmissionError "model not found")
          (
            \ms ->
              do
                (ms', sr) <- submitWork ms submission
                modifys $ \s@ServerState{..} -> s {models = nubBy ((==) `on` fst) $ (identifier, ms') : models}
                return sr
          )
          modelState'
    getRecords recordFilter identifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup identifier . models
        maybeModelNotFound
          (return . map record . filterRecordState recordFilter . works)
          modelState'
  in
    Service{..}


main :: IO ()
main = runService 8090 service =<< initialize


maybeModelNotFound :: (a -> ServerM s b) -> Maybe a -> ServerM s b
maybeModelNotFound = maybe $ throwError "model not found"


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

GET /server/MODEL/bookmark_metas TAG_ID multiple BOOKMARK_META

GET /server/MODEL/bookmarks bookmark_id=BOOKMARK_ID BOOKMARK

POST /server/MODEL_ID/bookmarks BOOKMARK w/o ID BOOKMARK_META

GET /server/MODEL_ID/filter_metas TAG_ID=TEXT multiple optional FILTER_META

GET /server/MODEL_ID/filters filter_id=FILTER_ID optional FILTER

POST /server/MODEL_ID/filters FILTER w/o ID FILTER_META

-}
