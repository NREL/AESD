{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Record (
  VarValue
, varValue
, RecordIdentifier
, RecordContent
, Record
, record
, RecordList
, recordList
, RecordTable
, variableIdentifiers
, recordIdentifiers
, realTable
, integerTable
, stringTable
, onTable
, recordTable
, RecordData
, list
, table
, recordData
, onRecordContent
) where


import CESDS.Types (DataValue, realValue, integerValue, onDataValue, stringValue)
import CESDS.Types.Internal (Doubles, Integers, Strings, reals, integers, strings)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Packed, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


type RecordIdentifier = Int64


data VarValue =
  VarValue
  {
    varIdentifier' :: Required 1 (Value   VariableIdentifier)
  , varValue'      :: Required 2 (Message DataValue         )
  }
    deriving (Generic, Show)

instance Default VarValue where
  def = VarValue (putField 0) (putField def) -- FIXME

instance Decode VarValue

instance Encode VarValue


varValue :: Lens' VarValue (VariableIdentifier, DataValue)
varValue =
  lens
    (\VarValue{..} -> (getField varIdentifier'      , getField varValue'    ))
    (\s (k, v)     -> s {varIdentifier' = putField k, varValue' = putField v})


data Record =
  Record
  {
    recordIdentifier' :: Required 1 (Value RecordIdentifier)
  , varValues'        :: Repeated 2 (Message VarValue      )
  }
    deriving (Generic, Show)

instance Default Record where
  def = Record (putField 0) (putField  def)

instance Decode Record

instance Encode Record


type RecordContent = (RecordIdentifier, [(VariableIdentifier, DataValue)])


record :: Lens' Record RecordContent
record =
  lens
    (\Record{..} -> (getField recordIdentifier'      , (^. varValue) <$> getField varValues'                ))
    (\s (k, v)   -> s {recordIdentifier' = putField k, varValues' = putField $ flip (varValue .~) def <$>  v})


data RecordList =
  RecordList
  {
    recordList' :: Repeated 1 (Message Record)
  }
    deriving (Generic, Show)

instance Default RecordList where
  def = RecordList $ putField []

instance Decode RecordList

instance Encode RecordList


recordList :: Lens' RecordList [RecordContent]
recordList =
  lens
    (\RecordList{..} -> (^. record) <$> getField recordList')
    (\s x         -> s {recordList' = putField $ flip (record .~) def <$> x})


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
  def = RecordTable (putField []) (putField []) (putField Nothing) (putField Nothing) (putField Nothing)

instance Decode RecordTable

instance Encode RecordTable


variableIdentifiers :: Lens' RecordTable [VariableIdentifier]
variableIdentifiers = lens (getField . variableIdentifiers')  (\s x -> s {variableIdentifiers' = putField x})


recordIdentifiers :: Lens' RecordTable [RecordIdentifier]
recordIdentifiers = lens (getField . recordIdentifiers')  (\s x -> s {recordIdentifiers' = putField x})


realTable:: Lens' RecordTable (Maybe [Double])
realTable =
  lens
    (fmap (^. reals) . getField . realTable')
    (\s x -> s {realTable' = putField $ flip (reals .~) def <$> x})


integerTable:: Lens' RecordTable (Maybe [Int64])
integerTable =
  lens
    (fmap (^. integers) . getField . integerTable')
    (\s x -> s {integerTable' = putField $ flip (integers .~) def <$> x})


stringTable:: Lens' RecordTable (Maybe [String])
stringTable =
  lens
    (fmap (^. strings) . getField . stringTable')
    (\s x -> s {stringTable' = putField $ flip (strings .~) def <$> x})


onTable :: ([Double] -> a) -> ([Int64] -> a) -> ([String] -> a) -> RecordTable -> Maybe a
onTable f g h x =
      f <$> x ^. realTable
  <|> g <$> x ^. integerTable
  <|> h <$> x ^. stringTable


recordTable :: RecordTable -> [RecordContent]
recordTable t =
  let
    relens :: Lens' RecordTable (Maybe [a]) -> Lens' DataValue (Maybe a) -> Maybe [DataValue]
    relens tlens dlens = fmap (flip (dlens .~) def . Just) <$> t ^. tlens
    vids = t ^. variableIdentifiers
    vals =
      fromMaybe []
         $  relens realTable    realValue
        <|> relens integerTable integerValue
        <|> relens stringTable  stringValue
  in
    fmap (second $ zip vids)
      . zip (t ^. recordIdentifiers)
      $ chunksOf (length vids) vals


data RecordData =
  RecordData
  {
    list'  :: Optional 1 (Message RecordList )
  , table' :: Optional 2 (Message RecordTable)
  }
    deriving (Generic, Show)

instance Decode RecordData

instance Encode RecordData


list :: Lens' RecordData (Maybe RecordList)
list = lens (getField . list') (\s x -> s {list' = putField x})


table :: Lens' RecordData(Maybe RecordTable)
table = lens (getField . table') (\s x -> s {table' = putField x})


recordData :: RecordData -> [RecordContent]
recordData x =
  fromMaybe []
     $  (^. recordList) <$> x ^. list
    <|> recordTable     <$> x ^. table


onRecordContent :: (Double -> a)
                -> (Int64  -> a)
                -> (String -> a)
                -> a
                -> [RecordContent]
                -> [(RecordIdentifier, [(VariableIdentifier, a)])]
onRecordContent f g h d =
  fmap
    . second
    $ fmap (second $ fromMaybe d . onDataValue f g h)
