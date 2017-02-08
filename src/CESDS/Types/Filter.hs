{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Filter (
  Filter(..)
, filterRecords
, FilterExpression
, toExpression
, fromExpression
) where


import CESDS.Types.Domain (DomainMeta, filterInterval, filterSet, interval, set, variable)
import CESDS.Types.Internal ()
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Value (DataValue)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Repeated, Required, getField, putField)
import GHC.Generics (Generic)



data Filter =
    Not Filter
  | Union [Filter]
  | Intersection [Filter]
  | Interval VariableIdentifier (Maybe DataValue, Maybe DataValue)
  | Set VariableIdentifier [DataValue]
    deriving (Eq, Read, Show)


filterRecords :: Filter -> [RecordContent] -> [RecordContent]
filterRecords = filter . filterRecord


filterRecord :: Filter -> RecordContent -> Bool
filterRecord (Not          f  ) r = not $ filterRecord f r
filterRecord (Union        fs ) r = any (`filterRecord` r) fs
filterRecord (Intersection fs ) r = all (`filterRecord` r) fs
filterRecord (Interval     v i) r = filterInterval v i r
filterRecord (Set          v s) r = filterSet      v s r


toExpression :: Filter -> FilterExpression
toExpression (Not   f         ) = def {filterNot'          = putField . Just . FilterNot          . putField $ toExpression     f }
toExpression (Union fs        ) = def {filterUnion'        = putField . Just . FilterUnion        . putField $ toExpression <$> fs}
toExpression (Intersection fs ) = def {filterIntersection' = putField . Just . FilterIntersection . putField $ toExpression <$> fs}
toExpression (Interval     v i) = def {filterDomain'       = putField . Just $ def & variable .~ v & interval .~ Just i           }
toExpression (Set          v s) = def {filterDomain'       = putField . Just $ def & variable .~ v & set      .~ Just s           }


fromExpression :: FilterExpression -> Filter
fromExpression FilterExpression{..} =
  fromMaybe (Union [])
    $  Not          .      fromExpression . getField . notExpression'           <$> getField filterNot'
   <|> Union        . fmap fromExpression . getField . unionExpressions'        <$> getField filterUnion'
   <|> Intersection . fmap fromExpression . getField . intersectionExpressions' <$> getField filterIntersection'
   <|> do
         d <- getField filterDomain'
         let
           v = d ^. variable
         Interval v <$> d ^. interval <|> Set v <$> d ^. set


data FilterExpression =
  FilterExpression
  {
    filterNot'          :: Optional 1 (Message FilterNot         )
  , filterUnion'        :: Optional 2 (Message FilterUnion       )
  , filterIntersection' :: Optional 3 (Message FilterIntersection)
  , filterDomain'       :: Optional 4 (Message DomainMeta        )
  }
    deriving (Generic, Show)

instance Default FilterExpression where
  def = FilterExpression (putField def) (putField def) (putField def) (putField def)

instance Decode FilterExpression

instance Encode FilterExpression


data FilterNot = 
  FilterNot
  {
    notExpression' :: Required 1 (Message FilterExpression)
  }
    deriving (Generic, Show)

instance Default FilterNot where
  def = FilterNot (putField def)

instance Decode FilterNot

instance Encode FilterNot


data FilterUnion =
  FilterUnion
  {
    unionExpressions' :: Repeated 1 (Message FilterExpression)
  }
    deriving (Generic, Show)

instance Default FilterUnion where
  def = FilterUnion (putField def)

instance Decode FilterUnion

instance Encode FilterUnion


data FilterIntersection =
  FilterIntersection
  {
    intersectionExpressions' :: Repeated 1 (Message FilterExpression)
  }
    deriving (Generic, Show)

instance Default FilterIntersection where
  def = FilterIntersection (putField def)

instance Decode FilterIntersection

instance Encode FilterIntersection
