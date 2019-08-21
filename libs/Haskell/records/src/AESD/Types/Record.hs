{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for records of data.
-}


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module AESD.Types.Record (
-- * Values of variables
  VarValue
, makeVarValue
, varValue
-- * Records
, RecordIdentifier
, RecordContent
, RecordData
, makeRecordData
, recordData
, onRecordContent
-- * Filtering
, filterRecords
, filterVariables
) where


import AESD.Types.Internal (Doubles, Integers, Strings, integers, makeIntegers, makeReals, makeStrings, reals, strings)
import AESD.Types.Value (DataValue, realValue, integerValue, onDataValue, onDataValues, sameVarTypes, stringValue)
import AESD.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~), (?~))
import Data.Default (Default(..))
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Packed, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


{-# ANN module "HLint: ignore Use newtype instead of data" #-}


-- | A value of a variable.
data VarValue =
  VarValue
  {
    varIdentifier' :: Required 1 (Value   VariableIdentifier)
  , varValue'      :: Required 2 (Message DataValue         )
  }
    deriving (Generic, Show)

instance Decode VarValue

instance Encode VarValue


-- | Construct a value for a variable.
{-# INLINE makeVarValue #-}
makeVarValue :: VariableIdentifier -- ^ The unique identifier for the variable.
             -> DataValue          -- ^ The value.
             -> VarValue           -- ^ The variable value.
makeVarValue identifier'' value'' =
  VarValue
  {
    varIdentifier' = putField identifier''
  , varValue'      = putField value''
  }


-- | Get the value for a variable.
{-# INLINE varValue #-}
varValue :: VarValue -> (VariableIdentifier, DataValue)
varValue VarValue{..} = (getField varIdentifier', getField varValue')


-- | A unique identifier for a record.
type RecordIdentifier = Int64


-- | A record, consisting of a unique identifier and variables paired with their values.
type RecordContent = (RecordIdentifier, [(VariableIdentifier, DataValue)])


-- FIXME: Consider eliminating lenses from the rest of this source file.


-- | A record.
data Record =
  Record
  {
    recordIdentifier' :: Required 1 (Value RecordIdentifier)
  , varValues'        :: Repeated 2 (Message VarValue      )
  }
    deriving (Generic, Show)

instance Default Record where
  def =
    Record
    {
      recordIdentifier' = mempty
    , varValues'        = mempty
    }

instance Decode Record

instance Encode Record


-- | Lens for a record.
record :: Lens' Record RecordContent
record =
  lens
    (\Record{..} -> (getField recordIdentifier'      , varValue <$> getField varValues'                  ))
    (\s (k, v)   -> s {recordIdentifier' = putField k, varValues' = putField $ uncurry makeVarValue <$> v})


-- | A list of records.
data RecordList =
  RecordList
  {
    recordList' :: Repeated 1 (Message Record)
  }
    deriving (Generic, Show)

instance Default RecordList where
  def = RecordList mempty

instance Decode RecordList

instance Encode RecordList


-- | Lens for a list of records.
recordList :: Lens' RecordList [RecordContent]
recordList =
  lens
    (\RecordList{..} -> (^. record) <$> getField recordList')
    (\s x         -> s {recordList' = putField $ flip (record .~) def <$> x})


-- | A table of records.
data RecordTable =
  RecordTable
  {
    variableIdentifiers' :: Packed   1 (Value   VariableIdentifier)
  , recordIdentifiers'   :: Packed   2 (Value   RecordIdentifier  )
  , realTable'           :: Optional 3 (Message Doubles           )
  , integerTable'        :: Optional 4 (Message Integers          )
  , stringTable'         :: Optional 5 (Message Strings           )
  }
    deriving (Generic, Show)

instance Default RecordTable where
  def =
    RecordTable
    {
      variableIdentifiers' = mempty
    , recordIdentifiers'   = mempty
    , realTable'           = mempty
    , integerTable'        = mempty
    , stringTable'         = mempty
    }

instance Decode RecordTable

instance Encode RecordTable


-- | Lens for the record identifiers in a table of records.
recordIdentifiers :: Lens' RecordTable [RecordIdentifier]
recordIdentifiers = lens (getField . recordIdentifiers')  (\s x -> s {recordIdentifiers' = putField x})


-- | Lens for the variable identifiers in a table of records.
variableIdentifiers :: Lens' RecordTable [VariableIdentifier]
variableIdentifiers = lens (getField . variableIdentifiers')  (\s x -> s {variableIdentifiers' = putField x})


-- | Lens for a table of real values.
realTable:: Lens' RecordTable (Maybe [Double])
realTable =
  lens
    (fmap reals . getField . realTable')
    (\s x -> s {realTable' = putField $ makeReals <$> x})


-- | Lens for a table of integer values.
integerTable:: Lens' RecordTable (Maybe [Int64])
integerTable =
  lens
    (fmap integers . getField . integerTable')
    (\s x -> s {integerTable' = putField $ makeIntegers <$> x})


-- | Lens for a table of string values.
stringTable:: Lens' RecordTable (Maybe [String])
stringTable =
  lens
    (fmap strings . getField . stringTable')
    (\s x -> s {stringTable' = putField $ makeStrings <$> x})


-- | Lens for the content of a table of records.
recordTable :: Lens' RecordTable [RecordContent]
recordTable =
  lens
    (
      \s ->
        let
          vids = s ^. variableIdentifiers
          vals =
            fromMaybe []
               $  fmap realValue    <$> s ^. realTable
              <|> fmap integerValue <$> s ^. integerTable
              <|> fmap stringValue  <$> s ^. stringTable
        in
          fmap (second $ zip vids)
            . zip (s ^. recordIdentifiers)
            $ chunksOf (length vids) vals
    )
    (
      \s x ->
        onDataValues
          (realTable    ?~)
          (integerTable ?~)
          (stringTable  ?~)
          (id :: RecordTable -> RecordTable )
          (fmap snd . snd =<< x :: [DataValue])
          $ s
          & variableIdentifiers .~ (if null x then [] else fst <$> snd (head x))
          & recordIdentifiers   .~ fmap fst x
    )


-- | Collections of records.
data RecordData =
  RecordData
  {
    list'  :: Optional 1 (Message RecordList )
  , table' :: Optional 2 (Message RecordTable)
  }
    deriving (Generic, Show)

instance Decode RecordData

instance Encode RecordData


-- | Construct a collection of records.
makeRecordData :: [RecordContent] -> RecordData
makeRecordData x
  | sameVarTypes $ fmap snd . snd =<< x = RecordData {list' = mempty, table' = putField . Just $ def & recordTable .~ x}
  | otherwise                           = RecordData {list' = putField . Just $ def & recordList .~ x,  table' = mempty}


-- | Get a collection of records.
recordData :: RecordData -> [RecordContent]
recordData x =
  fromMaybe []
     $  (^. recordList ) <$> getField (list'  x)
    <|> (^. recordTable) <$> getField (table' x)


-- | Apply functions to a collection of records.
--
-- For example:
--
-- import AESD.Types.Value (integerValue, realValue, stringValue)
-- >>> let rs = [(1, [(0, realValue 10), (1, integerValue 20), (2, stringValue "thirty")])]
-- >>> onRecordContent show show id "NA" rs
-- [(1,[(0,"10.0"),(1,"20"),(2,"thirty")])]
onRecordContent :: (Double -> a)                                    -- ^ Handle a real value.
                -> (Int64  -> a)                                    -- ^ Handle an integer value.
                -> (String -> a)                                    -- ^ Handle a string value.
                -> a                                                -- ^ The default result value.
                -> [RecordContent]                                  -- ^ The records.
                -> [(RecordIdentifier, [(VariableIdentifier, a)])]  -- ^ The result of applying the handler functions to the values in the records.
onRecordContent f g h d =
  fmap
    . second
    $ fmap (second $ onDataValue f g h d)


-- | Filter records by their identifiers.
filterRecords :: [RecordIdentifier] -- ^ The unique identifiers to be found.
              -> [RecordContent]    -- ^ The original records.
              -> [RecordContent]    -- ^ The records with the unique identifiers.
filterRecords rids = filter ((`elem` rids) . fst) -- FIXME: We might want to use a faster data structure here.


-- | Filter records by their variables.
filterVariables :: [VariableIdentifier] -- ^ The variables to be selected.
                -> [RecordContent]      -- ^ The original records.
                -> [RecordContent]      -- ^ The selected variables from the records.
filterVariables vids =
  fmap
    . second
    $ filter ((`elem` vids) . fst)
