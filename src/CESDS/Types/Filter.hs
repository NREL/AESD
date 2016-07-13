{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Filter (
  FilterIdentifier
, Filter(..)
, validateFilters
, validateFilter
, FilterList(..)
, makeFilterList
, validateFilterList
, SelectionExpression(..)
, validateSelectionExpression
) where


import CESDS.Types (Color, Identifier, Tags, Val, object')
import CESDS.Types.Variable (Domain(..), Variable, VariableIdentifier, canHaveVal, compatibleDomains, hasVariable)
import Control.Applicative ((<|>))
import Control.Monad ( when)
import Control.Monad.Except (MonadError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import Data.List.Util (deleteOn, noDuplicates, notDuplicatedIn)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified CESDS.Types.Variable as Variable (Variable(..))


type FilterIdentifier = Identifier


data Filter =
  Filter
  {
    identifier :: Maybe FilterIdentifier
  , name       :: Text
  , size       :: Maybe Int
  , color      :: Maybe Color
  , tags       :: Maybe Tags
  , expression :: Maybe SelectionExpression
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Filter where
  parseJSON =
    withObject "FILTER" $ \o ->
      do
        meta <- withObject "FILTER_META"
                  (\o' ->
                    do
                      identifier <- o' .:? "filter_id"
                      name       <- o' .:  "name"
                      size       <- o' .:? "size"
                      color      <- o' .:? "color"
                      tags       <- o' .:? "tags"
                      let expression = Nothing
                      return Filter{..}
                  )
                  =<< o .: "meta"
        expression <- o .:? "expr"
        return $ meta {expression = expression}

instance ToJSON Filter where
  toJSON Filter{..} =
    object'
      $ maybe id ((:) . ("expr" .=)) expression
      [
        "meta" .= object'
                    [
                      "filter_id" .= identifier
                    , "name"      .= name
                    , "size"      .= size
                    , "color"     .= color
                    , "tags"      .= tags
                    ]
      ]


validateFilters :: (IsString e, MonadError e m) => [Variable] -> [Filter] -> m ()
validateFilters variables filters =
  do
    assert "duplicate filter identifiers" $ noDuplicates $ map identifier filters
    sequence_
      [
        validateFilter (deleteOn identifier filter' filters) variables filter'
      |
        filter' <- filters
      ]


validateFilter :: (IsString e, MonadError e m) => [Filter] -> [Variable] -> Filter -> m ()
validateFilter filters variables filter' =
  do
    assert "duplicate filter identifier" $ notDuplicatedIn identifier filter' filters
    maybe
      (return ())
      (validateSelectionExpression variables)
      $ expression filter'


data FilterList =
  FilterList
  {
    count   :: Int
  , filters :: [FilterIdentifier]
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON FilterList where
  parseJSON =
    withObject "FILTER_META_LIST" $ \o ->
      do
        count   <- o .: "count"
        filters <- o .: "filter_ids"
        return FilterList{..}

instance ToJSON FilterList where
  toJSON FilterList{..} = object' ["count" .= count, "filter_ids" .= filters]


makeFilterList :: [Filter] -> FilterList
makeFilterList items =
  let
    filters = mapMaybe identifier items
    count = length filters
  in
    FilterList{..}

validateFilterList :: (IsString e, MonadError e m) => FilterList -> m ()
validateFilterList FilterList{..} =
  do
    assert "filter count does not match number of filters" $ count == length filters
    assert "duplicate filter identifiers" $ noDuplicates filters


data SelectionExpression =
    NotSelection
    {
      negatedExpression :: SelectionExpression
    }
  | UnionSelection
    {
      expressions :: [SelectionExpression]
    }
  | IntersectSelection
    {
      expressions :: [SelectionExpression]
    }
  | ValueSelection
    {
      variable :: VariableIdentifier
    , value    :: Val
    }
  | DomainSelection
    {
      variable :: VariableIdentifier
    , domain   :: Domain
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON SelectionExpression where
  parseJSON =
    withObject "SEL_EXPR" $ \o ->
      parseSet o <|> parseInterval o <|> parseValue o <|> parseNot o <|> parseUnion o <|> parseIntersect o
    where
      parseNot       o = NotSelection   <$> o .: "not"
      parseUnion     o = UnionSelection <$> o .: "union"
      parseIntersect o = IntersectSelection <$> o .: "isect"
      parseValue o =
        do
          variable <- o .: "var"
          value    <- o .: "value"
          return ValueSelection{..}
      parseInterval o =
        do
          variable <- o .: "var"
          interval <- o .: "interval"
          when (length interval /= 2)
            $ fail "SEL_EXPR interval must contain two entries"
          let
            [lowerBound, upperBound] = interval
            domain = Interval{..}
          return DomainSelection{..}
      parseSet o =
        do
          variable <- o .: "var"
          options  <- o .: "set"
          let
            domain = Set{..}
          return DomainSelection{..}

instance ToJSON SelectionExpression where
  toJSON NotSelection{..} =
    object'
      [
        "not" .= negatedExpression
      ]
  toJSON UnionSelection{..} =
    object'
      [
        "union" .= expressions
      ] 
  toJSON IntersectSelection{..} =
    object'
      [
        "isect" .= expressions
      ]
  toJSON ValueSelection{..} =
    object'
      [
        "var"   .= variable
      , "value" .= value
      ]
  toJSON DomainSelection{..} =
    case domain of
      Interval{..} -> object'
                        [
                          "var"      .= variable
                        , "interval" .= [lowerBound, upperBound]
                        ]
      Set{..}      -> object'
                        [
                          "var" .= variable
                        , "set" .= options
                        ]


validateSelectionExpression :: (IsString e, MonadError e m) => [Variable] -> SelectionExpression -> m ()
validateSelectionExpression variables NotSelection{..} =
  validateSelectionExpression variables negatedExpression
validateSelectionExpression variables UnionSelection{..} =
  mapM_ (validateSelectionExpression variables) expressions
validateSelectionExpression variables IntersectSelection{..} =
  mapM_ (validateSelectionExpression variables) expressions
validateSelectionExpression variables ValueSelection{..} =
  do
    variable' <- variables `hasVariable` variable
    Variable.domain variable' `canHaveVal` value
validateSelectionExpression variables DomainSelection{..} =
  do
    variable' <- variables `hasVariable` variable
    Variable.domain variable' `compatibleDomains` domain
