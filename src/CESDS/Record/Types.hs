module CESDS.Record.Types (
) where


import Data.Aeson (Object, Value)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Network.URI (URI)


data Variable =
  Variable
  {
    variableID :: VariableIdentifier
  , display    :: Display
  , domain     :: Domain
  , units      :: Maybe Units
  , isInput    :: Bool
  }

type VariableIdentifier = Text

data Display =
  Display
  {
    label      :: Text
  , shortLabel :: Maybe Text
  , color      :: Maybe Color
  }

type Color = String -- FIXME SVGcolor


data Domain =
    Interval
    {
      lowerBound :: Maybe Scientific
    , upperBound :: Maybe Scientific
    }
  | Set 
    {
      options :: [SetValue]
    }

type SetValue = Text


data Units =
  Units
  {
    lengthExponent      :: Int
  , massExponent        :: Int
  , timeExponent        :: Int
  , currentExponent     :: Int
  , temperatureExponent :: Int
  , molExponent         :: Int
  , intensityExponent   :: Int
  , angleExponent       :: Int
  , scale               :: Scientific
  }

data Model =
  Model
  {
    modelID          :: ModelIdentifier
  , modelURI         :: URI
  , modelName        :: Maybe Text
  , modelDescription :: Maybe Text
  , modelTags        :: Object
  , generation       :: Generation
  , variables        :: [VariableIdentifier]
  , primaryKey       :: VariableIdentifier
  , timeKey          :: Maybe VariableIdentifier
  }

type ModelIdentifier = Text

type Generation = Int

data Server =
  Server
  {
    serverID :: ServerIdentifier  -- what is the point of server ID?
  , version  :: APIVersion
  , models   :: [ModelIdentifier]
  , status   :: ServerStatus
  }

type ServerIdentifier = Text

type APIVersion = Int

data ServerStatus =
    Okay
  | Broken
  | OnFire
  | OtherStatus Text

data Command =
    RestartServer
  | RestartModel   ModelIdentifier
  | ClearServer
  | ClearModel     ModelIdentifier
  | StrategyRandom ModelIdentifier
  | StrategyFIFO   ModelIdentifier
  | StrategyFILO   ModelIdentifier


data CommandResult =
    CommandSuccess
  | CommandError String


type Record = Object
    

data WorkSubmission =
  WorkSubmission
  {
    explicitVariables :: [(VariableIdentifier, Scientific)]
  , randomVariables   :: [VariableIdentifier]
  , timeout           :: Maybe Duration
  , priority          :: Maybe Priority
  }

type Duration = Double

type Priority = Int


data WorkSubmissionResult =
    WorkSubmissionResult
    {
      workID              :: WorkIdentifier
    , generation'         :: Generation
    , estimatedCompletion :: Maybe Duration
    }
  | WorkSubmissionError
    {
      error :: Text
    }

type WorkIdentifier = Text

data WorkStatus =
    WorkPending
    {
      workID' :: WorkIdentifier
    }
  | WorkRunning
    {
      workID' :: WorkIdentifier
    }
  | WorkSuccess
    {
      workID'  :: WorkIdentifier
    , resultID :: ResultIdentifier
    }
  | WorkFailed
    {
      workID' :: WorkIdentifier
    , additional :: Text
    }

type ResultIdentifier = Text

data BookmarkMeta =
  BookmarkMeta
  {
    bookmarkID    :: BookmarkIdentifier
  , bookmarkName  :: Text
  , bookmarkSize  :: Int
  , bookmarkColor :: Maybe Color
  , bookmarkTags  :: Object
  }

data Bookmark =
  Bookmark
  {
    bookmarkMeta :: BookmarkMeta
  , bookmarkResults :: [ResultIdentifier]
  }

type BookmarkIdentifier = Text

data SelectionExpression =
    NotSelection 
    {
      expression1 :: SelectionExpression
    }
  | UnionSelection
    {
      expression1 :: SelectionExpression
    , expression2 :: SelectionExpression
    }
  | IntersectSelection
    {
      expression1 :: SelectionExpression
    , expression2 :: SelectionExpression
    }
  | ValueSelection
    {
      selectionVariable :: VariableIdentifier
    }
  | DomainSelection
    {
      selectionVariable :: VariableIdentifier
    , selectionDomain :: Domain
    }

data FilterMeta =
  FilterMeta
  {
    filterID    :: FilterIdentifier
  , filterName  :: Text
  , filterSize  :: Maybe Int
  , filterColor :: Maybe Color
  , filterTags  :: Object
  }

type FilterIdentifier = Text

data Filter =
  Filter
  {
    filterMeta :: FilterMeta
  , filterExpression :: SelectionExpression
  }
