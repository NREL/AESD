{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for filters on variables.
-}


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module AESD.Types.Filter (
-- * Filter trees
  Filter(..)
, filterRecord
, filterRecords
-- * Filter expressions
, FilterExpression
, toExpression
, fromExpression
) where


import AESD.Types.Domain (DomainMeta, filterInterval, filterSet, makeInterval, makeSet, onDomainMeta)
import AESD.Types.Internal ()
import AESD.Types.Record (RecordContent)
import AESD.Types.Value (DataValue)
import AESD.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Repeated, Required, getField, putField)
import GHC.Generics (Generic)



-- | A filter for records.
data Filter =
    Not Filter                                                     -- ^ Negation of a filter.
  | Union [Filter]                                                 -- ^ Union of filters.
  | Intersection [Filter]                                          -- ^ Intersection of filters.
  | Interval VariableIdentifier (Maybe DataValue, Maybe DataValue) -- ^ Filter on a variable by a (possible open) interval of its values.
  | Set VariableIdentifier [DataValue]                             -- ^ Filter on a variable by a set of its values.
    deriving (Eq, Read, Show)


-- | Apply a filter to records.
{-# INLINE filterRecords #-}
filterRecords :: Filter          -- ^ The filter.
              -> [RecordContent] -- ^ The original records.
              -> [RecordContent] -- ^ The records satisfying the filter.
filterRecords = filter . filterRecord


-- | Apply a filter to a record.
filterRecord :: Filter        -- ^ The filter.
             -> RecordContent -- ^ The record.
             -> Bool          -- ^ Whether the record satisfies the filter.
filterRecord (Not          f  ) r = not $ filterRecord f r
filterRecord (Union        fs ) r = any (`filterRecord` r) fs
filterRecord (Intersection fs ) r = all (`filterRecord` r) fs
filterRecord (Interval     v i) r = filterInterval v i r
filterRecord (Set          v s) r = filterSet      v s r


-- | An expression for filtering.  'FilterExpression' is used internally for encoding filters as protocol buffers.
data FilterExpression =
  FilterExpression
  {
    filterNot'          :: Optional 1 (Message FilterNot         )
  , filterUnion'        :: Optional 2 (Message FilterUnion       )
  , filterIntersection' :: Optional 3 (Message FilterIntersection)
  , filterDomain'       :: Optional 4 (Message DomainMeta        )
  }
    deriving (Generic, Show)

instance Decode FilterExpression

instance Encode FilterExpression


-- | A negation of an expression for filtering.
data FilterNot = 
  FilterNot
  {
    notExpression' :: Required 1 (Message FilterExpression)
  }
    deriving (Generic, Show)

instance Decode FilterNot

instance Encode FilterNot


-- | A union of expressions for filtering.
data FilterUnion =
  FilterUnion
  {
    unionExpressions' :: Repeated 1 (Message FilterExpression)
  }
    deriving (Generic, Show)

instance Decode FilterUnion

instance Encode FilterUnion


-- | An intersection of expressions for filtering.
data FilterIntersection =
  FilterIntersection
  {
    intersectionExpressions' :: Repeated 1 (Message FilterExpression)
  }
    deriving (Generic, Show)

instance Decode FilterIntersection

instance Encode FilterIntersection


-- | Convert a filter tree to an expression for filtering.
toExpression :: Filter -> FilterExpression
toExpression (Not   f         ) = def {filterNot'          = putField . Just . FilterNot          . putField $ toExpression     f }
toExpression (Union fs        ) = def {filterUnion'        = putField . Just . FilterUnion        . putField $ toExpression <$> fs}
toExpression (Intersection fs ) = def {filterIntersection' = putField . Just . FilterIntersection . putField $ toExpression <$> fs}
toExpression (Interval     v i) = def {filterDomain'       = putField . Just $ makeInterval v i                                   }
toExpression (Set          v s) = def {filterDomain'       = putField . Just $ makeSet      v s                                   }


-- An empty expression for filtering.
{-# INLINE def #-}
def :: FilterExpression
def = FilterExpression mempty mempty mempty mempty


-- | Convert an expression for filtering to a filter tree.
fromExpression :: FilterExpression -> Filter
fromExpression FilterExpression{..} =
  fromMaybe (Union [])
    $  Not          .      fromExpression . getField . notExpression'           <$> getField filterNot'
   <|> Union        . fmap fromExpression . getField . unionExpressions'        <$> getField filterUnion'
   <|> Intersection . fmap fromExpression . getField . intersectionExpressions' <$> getField filterIntersection'
   <|> (onDomainMeta ((return .) . Interval) ((return .) . Set) Nothing =<< getField filterDomain')
