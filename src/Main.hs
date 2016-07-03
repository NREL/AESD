{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Server (RecordFilter(..), ServerM, Service(..), WorkFilter(..), gets, modifys, modifysIO, runService, sets)
import CESDS.Types (Generation, Tags(..), isContinuous, isDiscrete, valAsString)
import CESDS.Types.Bookmark (Bookmark, BookmarkIdentifier)
import CESDS.Types.Bookmark.Test ()
import CESDS.Types.Filter (Filter, FilterIdentifier)
import CESDS.Types.Filter.Test ()
import CESDS.Types.Model (Model, ModelIdentifier)
import CESDS.Types.Model.Test (arbitraryModel)
import CESDS.Types.Record (Record, RecordIdentifier)
import CESDS.Types.Record.Test (arbitraryRecord)
import CESDS.Types.Server (Server)
import CESDS.Types.Server.Test ()
import CESDS.Types.Variable (isInterval, isSet)
import CESDS.Types.Variable.Test ()
import CESDS.Types.Work (Submission, WorkStatus, maybeRecordIdentifier)
import CESDS.Types.Work.Test ()
import Control.Arrow (second)
import Control.Monad (foldM, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.List (find, sort)
import Data.List.Util (disjoint, hasSubset, noDuplicates, nubOn, replaceByFst)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (pack)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, frequency, generate, listOf, suchThat)

import qualified CESDS.Types.Bookmark as Bookmark (Bookmark(..))
import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Filter as Filter (Filter(..))
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
  , bookmarks :: [Bookmark]
  , filters   :: [Filter]
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
    modelState <-
      ModelState
        <$> arbitraryModel modelIdentifier
        <*> return []
        <*> (nubOn Bookmark.identifier <$> listOf (suchThat arbitrary $ isJust . Bookmark.identifier))
        <*> (nubOn Filter.identifier   <$> listOf (suchThat arbitrary $ isJust . Filter.identifier  ))
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


submitWork :: ModelState -> Submission -> ServerM s (ModelState, Work.SubmissionResult)
submitWork ms@ModelState{..} submission =
  do
    let
      generation' = Model.generation model + 1
      modelVariables = map Variable.identifier $ Model.variables model
      explicitVariables' = sort . map fst $ Work.explicitVariables submission
      randomVariables' = sort $ Work.randomVariables submission
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
    unless (explicitVariables' `disjoint` randomVariables')
      $ throwError "explicit and random variables overlap"
    unless (map Variable.identifier (filter (isInterval . Variable.domain) $ Model.variables model) `hasSubset` map fst (filter (isContinuous . snd) $ Work.explicitVariables submission))
      $ throwError "continuous value specified for discrete variable"
    unless (map Variable.identifier (filter (isSet . Variable.domain) $ Model.variables model) `hasSubset` map fst (filter (isDiscrete . snd) $ Work.explicitVariables submission))
      $ throwError "discrete value specified for continuous variable"
    unless (key `notElem` explicitVariables' || fmap valAsString (lookup key $ Work.explicitVariables submission) `notElem` map Just recordKeys)
      $ throwError "primary key violation"
    ms' <- liftIO $ generate $ addArbitraryWork submission $ ms {model = model {Model.generation = generation'}}
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
    

addBookmark :: ModelState -> Bookmark -> ServerM ServerState (ModelState, Bookmark)
addBookmark ms@ModelState{..} bookmark =
  do
    -- FIXME: This implementation is incomplete.
    unless (Bookmark.identifier bookmark `notElem` map Bookmark.identifier bookmarks)
      $ throwError "bookmark already exists"
    unless (isJust $ Bookmark.records bookmark)
      $ throwError "record identifiers not specified for bookmark"
    unless (map recordIdentifier works `hasSubset` fromJust (Bookmark.records bookmark))
      $ throwError "invalid record identifiers"
    bookmarkIdentifier <- liftIO $ generate $ suchThat arbitrary (\x -> isJust x && x `notElem` map Bookmark.identifier bookmarks)
    let
      bookmark' = bookmark {Bookmark.identifier = bookmarkIdentifier}
    return (ms {bookmarks = bookmark' : bookmarks}, bookmark')


addFilter :: ModelState -> Filter -> ServerM ServerState (ModelState, Filter)
addFilter ms@ModelState{..} filter' =
  do
    -- FIXME: This implementation is incomplete.
    unless (Filter.identifier filter' `notElem` map Filter.identifier filters)
      $ throwError "filter already exists"
    filterIdentifier <- liftIO $ generate $ suchThat arbitrary (\x -> isJust x && x `notElem` map Filter.identifier filters)
    let
      filter'' = filter' {Filter.identifier = filterIdentifier}
    return (ms {filters = filter'' : filters}, filter'')


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


filterBookmarks :: Tags -> Maybe BookmarkIdentifier -> [Bookmark] -> [Bookmark]
filterBookmarks tags Nothing = filter ((`hasSubset` unTags tags) . unTags . Bookmark.tags)
filterBookmarks tags bookmarkIdentifier = filterBookmarks tags Nothing . filter ((== bookmarkIdentifier) . Bookmark.identifier)


filterFilters :: Tags -> Maybe FilterIdentifier -> [Filter] -> [Filter]
filterFilters tags Nothing = filter ((`hasSubset` unTags tags) . unTags . Filter.tags)
filterFilters tags filterIdentifier = filterFilters tags Nothing . filter ((== filterIdentifier) . Filter.identifier)


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
    postServer (Command.Clear _) =
      randomFailure
        $ do
          modifys $ \s@ServerState{..} -> s {models = map (second clearModel) models}
          return Command.Success
    postServer _ =
      notImplemented
    getModel modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          (return . model)
          modelState'
    postModel (Command.Restart _) modelIdentifier =
      randomFailure
        $ do
          model' <- gets $ lookup modelIdentifier . models
          maybe
            (return $ Command.Error "model not found" Nothing)
            (const $ return Command.Success)
            model'
    postModel (Command.Clear _) modelIdentifier =
      randomFailure
        $ do
          modelState' <- gets $ lookup modelIdentifier . models
          maybeModelNotFound
            (replaceModel . (, Command.Success) . clearModel)
            modelState'
    postModel _ _ =
      randomFailure
        $ return Command.Success
    getWorks workFilter modelIdentifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          (return . map workStatus . filterWorkState workFilter . works)
          modelState'
    postWork submission modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybe
          (return $ Work.SubmissionError "model not found")
          ((replaceModel =<<) . flip submitWork submission)
          modelState'
    getRecords recordFilter modelIdentifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          (return . map record . filterRecordState recordFilter . works)
          modelState'
    getBookmarkMetas tags modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          (return . map (\b -> b {Bookmark.records = Nothing}) . filterBookmarks tags Nothing . bookmarks)
          modelState'
    getBookmarks bookmarkIdentifier modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          (return . filterBookmarks (Tags []) bookmarkIdentifier . bookmarks)
          modelState'
    postBookmark bookmark modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          ((replaceModel =<<) . flip addBookmark bookmark)
          modelState'
    getFilterMetas tags modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          (return . map (\f -> f {Filter.expression = Nothing}) . filterFilters tags Nothing . filters)
          modelState'
    getFilters filterIdentifier modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          (return . filterFilters (Tags []) filterIdentifier . filters)
          modelState'
    postFilter filter' modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeModelNotFound
          ((replaceModel =<<) . flip addFilter filter')
          modelState'
  in
    Service{..}


clearModel :: ModelState -> ModelState
clearModel modelState = modelState {works = [], bookmarks = [], filters = []}


replaceModel :: (ModelState, a) -> ServerM ServerState a
replaceModel (modelState@ModelState{..}, x) =
  modifys (\s@ServerState{..} -> s {models = replaceByFst (Model.identifier model, modelState) models})
    >> return x


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
      else return . Command.Error "failed" $ Just "random failure for testing"


randomOutcome :: Int -> ServerM ServerState Bool
randomOutcome success =
  liftIO
    . generate
    $ frequency [(success, return True), (1, return False)]
