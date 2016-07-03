{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Filter (
  FilterIdentifier
, Filter(..)
, SelectionExpression(..)
) where


import CESDS.Types (Color, Identifier, Tags, Val, object')
import CESDS.Types.Variable (Domain(..), VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import GHC.Generics (Generic)


type FilterIdentifier = Identifier


data Filter =
  Filter
  {
    identifier :: Maybe FilterIdentifier
  , name       :: Text
  , size       :: Maybe Int
  , color      :: Maybe Color
  , tags       :: Tags
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
                      tags       <- o' .:  "tags"
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


data SelectionExpression =
    NotSelection 
    {
      right :: SelectionExpression
    }
  | UnionSelection
    {
      left  :: SelectionExpression
    , right :: SelectionExpression
    }
  | IntersectSelection
    {
      left  :: SelectionExpression
    , right :: SelectionExpression
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
      parseSet o <|> parseInterval o <|> parseValue o <|> parseExpression o
    where
      parseExpression o =
        do
          expr <- o .: "expr"
          case expr :: String of
            "not"   -> NotSelection       <$> o .: "a"
            "union" -> UnionSelection     <$> o .: "a" <*> o .: "b"
            "isect" -> IntersectSelection <$> o .: "a" <*> o .: "b"
            _       -> fail $ "invalid SEL_EXPR_TYPE \"" ++ expr ++ "\""
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
        "expr" .= pack "not"
      , "a"    .= right
      ]
  toJSON UnionSelection{..} =
    object'
      [
        "expr" .= pack "union"
      , "a"    .= left
      , "b"    .= right
      ] 
  toJSON IntersectSelection{..} =
    object'
      [
        "expr" .= pack "isect"
      , "a"    .= left
      , "b"    .= right
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
                        , "set" .= fromMaybe [] options
                        ]
