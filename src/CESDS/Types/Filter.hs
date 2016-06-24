{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module CESDS.Types.Filter (
  FilterIdentifier
, Filter(..)
, SelectionExpression(..)
) where


import CESDS.Types (Color, Identifier, Tags)
import CESDS.Types.Variable (Domain, VariableIdentifier)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


type FilterIdentifier = Identifier


data Filter =
  Filter
  {
    identifier :: FilterIdentifier
  , name       :: Text
  , size       :: Maybe Int
  , color      :: Maybe Color
  , tags       :: Tags
  , expression :: SelectionExpression
  }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)


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
      variable :: VariableIdentifier
    }
  | DomainSelection
    {
      variable :: VariableIdentifier
    , domain :: Domain
    }
    deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)
