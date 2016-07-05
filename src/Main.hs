{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Server (RecordFilter(..), ServerM, Service(..), WorkFilter(..), gets, modifys, modifysIO, runService, sets)
import CESDS.Types (Generation, Tags(..), valAsString)
import CESDS.Types.Bookmark (Bookmark, BookmarkIdentifier, validateBookmark, validateBookmarks)
import CESDS.Types.Bookmark.Test (arbitraryBookmark)
import CESDS.Types.Filter (Filter, FilterIdentifier, validateFilter, validateFilters)
import CESDS.Types.Filter.Test (arbitraryFilter)
import CESDS.Types.Model (Model, ModelIdentifier, validateModel)
import CESDS.Types.Model.Test (arbitraryModel)
import CESDS.Types.Record (Record, RecordIdentifier)
import CESDS.Types.Record.Test (arbitraryRecord)
import CESDS.Types.Server (Server, validateServer)
import CESDS.Types.Server.Test ()
import CESDS.Types.Variable.Test ()
import CESDS.Types.Work (Submission, WorkStatus, maybeRecordIdentifier, validateSubmission, validateWorkStatuses)
import CESDS.Types.Work.Test (arbitrarySubmission)
import Control.Arrow (second)
import Control.Monad (foldM, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.List (find)
import Data.List.Util (hasSubset, noDuplicates, nubOn, replaceByFst)
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


validateServerState :: ServerState -> ServerM s ()
validateServerState ServerState{..} =
  do
    validateServer server
    unless (noDuplicates $ map fst models)
      $ throwError "duplicate model identifiers"
    sequence_
      [
        do
          unless (modelIdentifier == Model.identifier model)
            $ throwError "corrupt model index"
          validateModelState modelState
      |
        (modelIdentifier, modelState@ModelState{..}) <- models
      ]


data ModelState =
  ModelState
  {
    model     :: Model
  , works     :: [WorkState]
  , bookmarks :: [Bookmark]
  , filters   :: [Filter]
  }
    deriving (Eq, Read, Show)


validateModelState :: ModelState -> ServerM s ()
validateModelState ModelState{..} =
  do
    validateModel [] model
--  mapM_ (validateSubmission model (map recordKey works)) $ map submission works
    validateWorkStatuses $ map workStatus works
    let
      recordIdentifiers = map recordIdentifier works
    validateBookmarks recordIdentifiers bookmarks
    validateFilters (Model.variables model) filters


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
    modelState <- ModelState <$> arbitraryModel modelIdentifier <*> return [] <*> return [] <*> return []
    submission <- arbitrarySubmission . Model.variables $ model modelState
    n <- choose (0, 4) :: Gen Int
    modelState' <- foldM (const . addArbitraryWork submission) modelState [1..n]
    let
      records = map recordIdentifier $ works modelState'
    bookmarks' <- nubOn Bookmark.identifier . filter ((> 0) . Bookmark.size) <$> listOf (arbitraryBookmark [Nothing] records)
    filters'   <- nubOn Filter.identifier   <$> listOf (arbitraryFilter [Nothing] . Model.variables $ model modelState)
    return modelState' {bookmarks = bookmarks', filters = filters'}


addArbitraryWork :: Work.Submission -> ModelState -> Gen ModelState
addArbitraryWork submission@Work.Submission{..} modelState@ModelState{..} =
  do
    let
      Model.Model{..} = model
      workGeneration = generation
      submissionKey = maybe "" (valAsString . snd) $ find ((== primaryKey) . fst) explicitVariables
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
    if submissionKey `elem` map Main.recordKey works
        || fWorkIdentifier w `elem` map (Work.workIdentifier . Main.workStatus) works
        || fRecordIdentifier w `elem` map fRecordIdentifier works
        || recordKey `elem` map Main.recordKey works
      then do
              f <- choose (0, 1) :: Gen Double
              if f < 0.1
                then return modelState
                else addArbitraryWork submission modelState
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
    validateSubmission model (map recordKey works) submission
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
    validateBookmark bookmarks (map recordIdentifier works) bookmark
    bookmarkIdentifier <- liftIO $ generate $ arbitrary `suchThat` (\x -> isJust x && x `notElem` map Bookmark.identifier bookmarks)
    let
      bookmark' = bookmark {Bookmark.identifier = bookmarkIdentifier, Bookmark.size = length $ Bookmark.records bookmark}
    return (ms {bookmarks = bookmark' : bookmarks}, bookmark')


addFilter :: ModelState -> Filter -> ServerM ServerState (ModelState, Filter)
addFilter ms@ModelState{..} filter' =
  do
    validateFilter filters (Model.variables model) filter'
    filterIdentifier <- liftIO $ generate $ arbitrary `suchThat` (\x -> isJust x && x `notElem` map Filter.identifier filters)
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
initialize =
  do
    s@ServerState{..} <- generate arbitrary
    return s {server = server {Server.version = 1}}


service :: Service ServerState
service =
  let
    getServer =
      do
        serverState@ServerState{..} <- gets id
        validateServerState serverState
        return server
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
