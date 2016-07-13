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
import CESDS.Types.Command.Test ()
import CESDS.Types.Filter (Filter, FilterIdentifier, validateFilter, validateFilters)
import CESDS.Types.Filter.Test (arbitraryFilter)
import CESDS.Types.Model (Model, ModelIdentifier, validateModel)
import CESDS.Types.Model.Test (arbitraryModel)
import CESDS.Types.Record (Record, RecordIdentifier)
import CESDS.Types.Record.Test (arbitraryRecord)
import CESDS.Types.Server (Server, validateServer)
import CESDS.Types.Server.Test ()
import CESDS.Types.Variable.Test ()
import CESDS.Types.Work (Submission, Work, maybeRecordIdentifier, validateSubmission, validateWorkList)
import CESDS.Types.Work.Test (arbitrarySubmission)
import Control.Arrow (second)
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.Except.Util (assert)
import Control.Monad.Reader (liftIO)
import Data.List (find)
import Data.List.Util (hasSubset, noDuplicates, nubOn, replaceByFst)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (pack)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, frequency, generate, listOf, suchThat)

import qualified CESDS.Types.Bookmark as Bookmark (Bookmark(..), makeBookmarkList)
import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Filter as Filter (Filter(..), makeFilterList)
import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Record as Record (Record(..), makeRecordList)
import qualified CESDS.Types.Server as Server (Server(..))
import qualified CESDS.Types.Work as Work (Submission(..), SubmissionResult(..), Work(..), hasStatus, isSuccess, makeWorkList)


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
    assert "duplicate model identifiers" $ noDuplicates $ map fst models
    sequence_
      [
        do
          assert "corrupt model index" $ modelIdentifier == Model.identifier model
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
    validateWorkList . Work.makeWorkList $ map work works
    let
      recordIdentifiers = map recordIdentifier works
    assert "inconsistent record count" $ Model.recordCount model == length recordIdentifiers
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
                work = case work of
                  Work.Pending{..} -> if r < 0.25 then Work.Running workIdentifier else work
                  Work.Running{..} -> if r < 0.50
                                        then if r < 0.10
                                               then Work.Failure workIdentifier "random failure"
                                               else Work.Success workIdentifier recordIdentifier
                                        else work
                  _                -> work
              }
        |
          workState@WorkState{..} <- works
        ]
    return $ updateRecordCount modelState {works = works'}


updateRecordCount :: ModelState -> ModelState
updateRecordCount ms@ModelState{..} = ms { model = model {Model.recordCount = length works}}


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
    return $ updateRecordCount modelState' {bookmarks = bookmarks', filters = filters'}


addArbitraryWork :: Work.Submission -> ModelState -> Gen ModelState
addArbitraryWork submission@Work.Submission{..} modelState@ModelState{..} =
  do
    let
      Model.Model{..} = model
      workGeneration = generation
      submissionKey = maybe "" (valAsString . snd) $ find ((== primaryKey) . fst) explicitVariables
    work <- arbitrary
    recordIdentifier <- maybe arbitrary return $ maybeRecordIdentifier work
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
      fWorkIdentifier = Work.workIdentifier . Main.work
      fRecordIdentifier = maybeRecordIdentifier . Main.work
    if submissionKey `elem` map Main.recordKey works
        || fWorkIdentifier w `elem` map fWorkIdentifier works
        || fRecordIdentifier w `elem` map fRecordIdentifier works
        || recordKey `elem` map Main.recordKey works
      then do
              f <- choose (0, 1) :: Gen Double
              if f < 0.05
                then return modelState
                else addArbitraryWork submission modelState
      else return modelState {works = w : works}


data WorkState =
  WorkState
  {
    workGeneration   :: Generation
  , work             :: Work
  , recordIdentifier :: RecordIdentifier
  , recordKey        :: String
  , record           :: Record
  }
    deriving (Eq, Read, Show)


submitWork :: ModelState -> Submission -> ServerM s (ModelState, Work.SubmissionResult)
submitWork ms submission =
  do
    let
      generation' = Model.generation (model ms) + 1
    validateSubmission (model ms) (map recordKey $ works ms) submission
    ms' <- liftIO $ generate $ addArbitraryWork submission $ ms {model = (model ms) {Model.generation = generation'}}
    assert "failed to submit new work" $ works ms == works ms'
    return
      (
        ms'
      , Work.Submitted
        {
          identifier          = Work.workIdentifier . work $ head $ works ms'
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
        && maybeCompare wfWork (==) (Work.workIdentifier work)
        && maybe True (flip Work.hasStatus work . pack) wfStatus


filterRecordState :: RecordFilter -> [WorkState] -> [WorkState]
filterRecordState RecordFilter{..} = -- FIXME: Also filter variable names.
  filter f
    where
      f WorkState{..} =
           Work.isSuccess work
        && maybeCompare rfFrom   (<=) workGeneration
        && maybeCompare rfTo     (>=) workGeneration
        && maybeCompare rfRecord (==) recordIdentifier 
        && maybeCompare rfKey    (==) recordKey


filterBookmarks :: Tags -> Maybe BookmarkIdentifier -> [Bookmark] -> [Bookmark]
filterBookmarks tags Nothing = filter ((`hasSubset` unTags tags) . unTags . fromMaybe (Tags []) . Bookmark.tags)
filterBookmarks tags bookmarkIdentifier = filterBookmarks tags Nothing . filter ((== bookmarkIdentifier) . Bookmark.identifier)


filterFilters :: Tags -> Maybe FilterIdentifier -> [Filter] -> [Filter]
filterFilters tags Nothing = filter ((`hasSubset` unTags tags) . unTags . fromMaybe (Tags []) . Filter.tags)
filterFilters tags filterIdentifier = filterFilters tags Nothing . filter ((== filterIdentifier) . Filter.identifier)


initialize :: IO ServerState
initialize = generate arbitrary


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
          randomResult
    postServer (Command.Clear _) =
      randomFailure
        $ do
          modifys $ \s@ServerState{..} -> s {models = map (second clearModel) models}
          randomResult
    postServer _ = throwError "bad request"
    getModel modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (return . model)
          modelState'
    postModel (Command.Restart _) modelIdentifier =
      randomFailure
        $ do
          model' <- gets $ lookup modelIdentifier . models
          maybeNotFound "model"
            (const . return . Command.Result $ Just "model successfully cleared")
            model'
    postModel (Command.Clear _) modelIdentifier =
      randomFailure
        $ do
          modelState' <- gets $ lookup modelIdentifier . models
          maybeNotFound "model"
            (replaceModel . (, Command.Result (Just "model successfully cleared")) . clearModel)
            modelState'
    postModel (Command.SetStrategy _) _ =
      randomFailure
        $ return (Command.Result $ Just "strategy request ignored")
    postModel (Command.GetStrategy _) _ =
      randomFailure
        $ return (Command.Result $ Just "default strategy")
    getRecord recordFilter modelIdentifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (
            (\records -> if null records then throwError "record not found" else return $ head records)
              . map record . filterRecordState recordFilter . works
          )
          modelState'
    getRecords recordFilter modelIdentifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (return . Record.makeRecordList . map record . filterRecordState recordFilter . works)
          modelState'
    postRecord _record modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (const $ throwError "not supported")
          modelState'
    getWork workFilter modelIdentifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (
            (\works' -> if null works' then throwError "work not found" else return $ head works')
              . map work . filterWorkState workFilter . works
          )
          modelState'
    getWorks workFilter modelIdentifier =
      do
        modifysIO ageModels
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (return . Work.makeWorkList . map work . filterWorkState workFilter . works)
          modelState'
    postWork submission modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          ((replaceModel =<<) . flip submitWork submission)
          modelState'
    deleteWork _workFilter modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (const $ throwError "not supported")
          modelState'
    getBookmarkList tags modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (return . Bookmark.makeBookmarkList . map (\b -> b {Bookmark.records = Nothing}) . filterBookmarks tags Nothing . bookmarks)
          modelState'
    getBookmark bookmarkIdentifier modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (
            (\bookmarks' -> if null bookmarks' then throwError "bookmark not found" else return $ head bookmarks')
              . filterBookmarks (Tags []) (Just bookmarkIdentifier) . bookmarks
          )
          modelState'
    postBookmark bookmark modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          ((replaceModel =<<) . flip addBookmark bookmark)
          modelState'
    deleteBookmark _bookmarkIdentifier modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (const $ throwError "not supported")
          modelState'
    getFilterList tags modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (return . Filter.makeFilterList . map (\f -> f {Filter.expression = Nothing}) . filterFilters tags Nothing . filters)
          modelState'
    getFilter filterIdentifier modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (
            (\filters' -> if null filters' then throwError "filter not found" else return $ head filters')
              . filterFilters (Tags []) (Just filterIdentifier) . filters
          )
          modelState'
    postFilter filter' modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          ((replaceModel =<<) . flip addFilter filter')
          modelState'
    deleteFilter _filterIdentifier modelIdentifier =
      do
        modelState' <- gets $ lookup modelIdentifier . models
        maybeNotFound "model"
          (const $ throwError "not supported")
          modelState'
  in
    Service{..}


clearModel :: ModelState -> ModelState
clearModel modelState = updateRecordCount $ modelState {works = [], bookmarks = [], filters = []}


replaceModel :: (ModelState, a) -> ServerM ServerState a
replaceModel (modelState@ModelState{..}, x) =
  modifys (\s@ServerState{..} -> s {models = replaceByFst (Model.identifier model, modelState) models})
    >> return x


main :: IO ()
main = runService 8090 service =<< initialize


maybeNotFound :: String -> (a -> ServerM s b) -> Maybe a -> ServerM s b
maybeNotFound s = maybe $ throwError (s ++ " not found")


randomResult :: ServerM ServerState Command.Result
randomResult = liftIO $ generate arbitrary


randomFailure :: ServerM ServerState Command.Result -> ServerM ServerState Command.Result
randomFailure x =
  do
    outcome <- randomOutcome 9
    if outcome
      then x
      else return . Command.Result $ Just "random failure for testing"


randomOutcome :: Int -> ServerM ServerState Bool
randomOutcome success =
  liftIO
    . generate
    $ frequency [(success, return True), (1, return False)]
