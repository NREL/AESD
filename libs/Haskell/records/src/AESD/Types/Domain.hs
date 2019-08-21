{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for domains of variables.

Use 'makeInterval' or 'makeSet' to construct a domain, and 'variable' or 'onDomainMeta' to access its contents.  Records can be filtered by domain using 'filterDomain' and 'filterDomains'
-}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}


module AESD.Types.Domain (
-- * Variable domains
  DomainMeta
, makeInterval
, makeSet
, variable
, onDomainMeta
-- * Filtering
, filterDomain
, filterDomains
, filterInterval
, filterSet
) where


import AESD.Types.Internal ()
import AESD.Types.Record (RecordContent)
import AESD.Types.Value (DataValue)
import AESD.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe, isJust)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


-- | Metadata for the domain of a variable.
data DomainMeta =
  DomainMeta
  {
    variable' :: Required 1 (Value   VariableIdentifier)
  , interval' :: Optional 2 (Message VarInterval       )
  , set'      :: Optional 3 (Message VarSet            )
  }
    deriving (Generic, Show)

instance Decode DomainMeta

instance Encode DomainMeta


-- | Construct a variable's domain.
{-# INLINE makeDomain #-}
makeDomain :: VariableIdentifier -- ^ The identifier for the variable in question.
           -> Maybe VarInterval  -- ^ Maybe the interval of values in the domain.  The interval may be open on either side of the interval.
           -> Maybe VarSet       -- ^ Maybe the set of values in the domain.
           -> DomainMeta         -- ^ The variable's domain.
makeDomain variable'' interval'' set'' =
  DomainMeta
  {
    variable' = putField variable''
  , interval' = putField interval''
  , set'      = putField set''
  }


-- | Construct a domain for an interval of values.
makeInterval :: VariableIdentifier                 -- ^ The identifier for the variable in question.
             -> (Maybe DataValue, Maybe DataValue) -- ^ The interval of values in the domain.  The interval may be open on either side of the interval.
             -> DomainMeta                         -- ^ The variable's domain.
makeInterval variable'' (firstValue'', lastValue'') =
  makeDomain variable''
    (
      Just
        VarInterval
        {
          firstValue' = putField firstValue''
        , lastValue'  = putField lastValue''
        }
    )
    Nothing


-- | Construct a domain for a set of values.
makeSet :: VariableIdentifier -- ^ The identifier for the variable in question.
        -> [DataValue]        -- ^ The set of values in the domain.
        -> DomainMeta         -- ^ The variable's domain.
makeSet variable'' set'' =
  makeDomain variable''
    Nothing
    (
      Just
        VarSet
        {
          values' = putField set''
        }
    )


-- | Get the variable identifier for a domain.
{-# INLINE variable #-}
variable :: DomainMeta -> VariableIdentifier
variable = getField . variable'


-- | Apply a function to a variable's domain.  A valid domain will only specify an interval or a set, so only one of the handler function will be applied.
--
-- For example:
--
-- >>> import AESD.Types.Domain (onDomainMeta)
-- >>> import AESD.Types.Value (realValue)
-- >>>
-- >>> let d = makeSet 42 $ realValue <$> [1066, 1812, 1939]
-- >>> onDomainMeta (const $ const "interval") (const $ const "set") "unknown" d
-- "set"
-- >>> onDomainMeta (const $ const 0) (const length) 0 d
-- 3
onDomainMeta :: (VariableIdentifier -> (Maybe DataValue, Maybe DataValue) -> a) -- ^ Handle an interval of values.
             -> (VariableIdentifier -> [DataValue] -> a)                        -- ^ Handle a set of values.
             -> a                                                               -- ^ The default result.
             -> DomainMeta                                                      -- ^ The variable's domain.
             -> a                                                               -- ^ The result of applying the first applicable handler to the domain.
onDomainMeta f g d x =
  fromMaybe d
    $  f i . (getField . firstValue' &&& getField . lastValue') <$> getField (interval' x)
   <|> g i .  getField . values'                                <$> getField (set'      x)
    where
      i = variable x


-- | An interval of values.
data VarInterval =
  VarInterval
  {
    firstValue' :: Optional 1 (Message DataValue)
  , lastValue'  :: Optional 2 (Message DataValue)
  }
    deriving (Generic, Show)

instance Decode VarInterval

instance Encode VarInterval


-- | Filter records based on an interval of values.
filterInterval :: VariableIdentifier                 -- ^ The variable's identifier.
               -> (Maybe DataValue, Maybe DataValue) -- ^ The interval of values, possibly open.
               -> RecordContent                      -- ^ The record.
               -> Bool                               -- ^ Whether the variable in the record is in the interval.
filterInterval i (Nothing, Nothing) = isJust                                  . lookup i . snd
filterInterval i (Just x , Nothing) = maybe False              (x <=)         . lookup i . snd
filterInterval i (Nothing, Just y ) = maybe False                     (>= y)  . lookup i . snd
filterInterval i (Just x , Just y ) = maybe False (liftA2 (&&) (x <=) (<= y)) . lookup i . snd


-- | A set of values.
data VarSet =
  VarSet
  {
     values' :: Repeated 1 (Message DataValue)
  }
    deriving (Generic, Show)

instance Decode VarSet

instance Encode VarSet


-- | Filter records based on a set of values.
filterSet :: VariableIdentifier -- ^ The variable's identifier.
          -> [DataValue]        -- ^ The set of values.
          -> RecordContent      -- ^ The record.
          -> Bool               -- ^ Whether the variable in the record is in the set.
filterSet i vs = maybe False (`elem` vs) . lookup i . snd


-- | Filter records based on a variable's domeain.
filterDomain :: DomainMeta      -- ^ The variable's domain.
             -> [RecordContent] -- ^ The original records.
             -> [RecordContent] -- ^ Teh records with their variable in th domain.
filterDomain =
  (filter .)
    . onDomainMeta filterInterval filterSet
    $ const True


-- | Filter records based on variables' domains.
filterDomains :: [DomainMeta]    -- ^ The variables' domains.
              -> [RecordContent] -- ^ The original records.
              -> [RecordContent] -- ^ The records with their variables in the domains.
filterDomains ds =
  let
    fs = onDomainMeta filterInterval filterSet (const True) <$> ds
  in
    filter $ flip all fs . flip id
