{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}


module CESDS.Types.Domain (
  DomainMeta
, variable
, interval
, set
, filterInterval
, filterSet
, filterDomain
, filterDomains
) where


import CESDS.Types.Internal ()
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Value (DataValue)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Maybe (fromMaybe, isJust)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


data VarInterval =
  VarInterval
  {
    firstValue' :: Optional 1 (Message DataValue)
  , lastValue'  :: Optional 2 (Message DataValue)
  }
    deriving (Generic, Show)

instance Default VarInterval where
  def = VarInterval (putField def) (putField def)

instance Decode VarInterval

instance Encode VarInterval


varInterval :: Lens' VarInterval (Maybe DataValue, Maybe DataValue)
varInterval =
  lens
    (getField . firstValue' &&& getField . lastValue')
    (\s (x, y) -> s {firstValue' = putField x, lastValue' = putField y})


filterInterval :: VariableIdentifier -> (Maybe DataValue, Maybe DataValue) -> RecordContent -> Bool
filterInterval i (Nothing, Nothing) = isJust                                  . lookup i . snd
filterInterval i (Just x , Nothing) = maybe False              (x <=)         . lookup i . snd
filterInterval i (Nothing, Just y ) = maybe False                     (>= y)  . lookup i . snd
filterInterval i (Just x , Just y ) = maybe False (liftA2 (&&) (x <=) (<= y)) . lookup i . snd


data VarSet =
  VarSet
  {
     values' :: Repeated 1 (Message DataValue)
  }
    deriving (Generic, Show)

instance Default VarSet where
  def = VarSet $ putField []

instance Decode VarSet

instance Encode VarSet


varSet :: Lens' VarSet [DataValue]
varSet = lens (getField . values') (\s x -> s {values' = putField x})


filterSet :: VariableIdentifier -> [DataValue] -> RecordContent -> Bool
filterSet i vs = maybe False (`elem` vs) . lookup i . snd


data DomainMeta =
  DomainMeta
  {
    variable' :: Required 1 (Value   VariableIdentifier)
  , interval' :: Optional 2 (Message VarInterval       )
  , set'      :: Optional 3 (Message VarSet            )
  }
    deriving (Generic, Show)

instance Default DomainMeta where
  def = DomainMeta (putField def) (putField def) (putField def)

instance Decode DomainMeta

instance Encode DomainMeta


variable :: Lens' DomainMeta VariableIdentifier
variable = lens (getField . variable') (\s x -> s {variable' = putField x})


interval :: Lens' DomainMeta (Maybe (Maybe DataValue, Maybe DataValue))
interval =
  lens
    (fmap (^. varInterval) . getField . interval')
    (\s x -> s {interval' = putField $ flip (varInterval .~) def <$> x})


set :: Lens' DomainMeta (Maybe [DataValue])
set =
  lens
    (fmap (^. varSet) . getField . set')
    (\s x -> s {set' = putField $ flip (varSet .~) def <$> x})


onDomainMeta :: (VariableIdentifier -> (Maybe DataValue, Maybe DataValue) -> a) -> (VariableIdentifier -> [DataValue] -> a) -> a -> DomainMeta -> a
onDomainMeta f g d x =
  fromMaybe d
    $  f i <$> x ^. interval
   <|> g i <$> x ^. set
    where
      i = x ^. variable


filterDomains :: [DomainMeta] -> [RecordContent] -> [RecordContent]
filterDomains ds =
  let
    fs = onDomainMeta filterInterval filterSet (const True) <$> ds
  in
    filter $ flip all fs . flip id


filterDomain :: DomainMeta -> [RecordContent] -> [RecordContent]
filterDomain =
  (filter .)
    . onDomainMeta filterInterval filterSet
    $ const True
