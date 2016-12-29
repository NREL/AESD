{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Types.Record (
  VarValue
, varValue
, RecordIdentifier
, RecordContent
, RecordData
, recordData
, onRecordContent
) where


import CESDS.Types.Internal (Doubles, Integers, Strings, reals, integers, strings)
import CESDS.Types.Value (DataValue, realValue, integerValue, onDataValue, sameVarTypes, stringValue)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~))
import Data.Default (Default(..))
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers (Decode, Encode, Message, Optional, Packed, Repeated, Required, Value, getField, putField)
import GHC.Generics (Generic)


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


type RecordIdentifier = Int64


type RecordContent = (RecordIdentifier, [(VariableIdentifier, DataValue)])


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


recordTable :: Lens' RecordTable [RecordContent]
recordTable =
  lens
    (
      \s ->
        let
          relens :: Lens' RecordTable (Maybe [a]) -> Lens' DataValue (Maybe a) -> Maybe [DataValue]
          relens tlens dlens = fmap (flip (dlens .~) def . Just) <$> s ^. tlens
          vids = s ^. variableIdentifiers
          vals =
            fromMaybe []
               $  relens realTable    realValue
              <|> relens integerTable integerValue
              <|> relens stringTable  stringValue
        in
          fmap (second $ zip vids)
            . zip (s ^. recordIdentifiers)
            $ chunksOf (length vids) vals
    )
    (
      \s x ->
        let
          y = fmap snd . snd =<< x
        in
          s
            & variableIdentifiers .~ (if null x then [] else fst <$> snd (head x))
            & recordIdentifiers   .~ fmap fst x
            & realTable           .~ mapM (^. realValue   ) y
            & integerTable        .~ mapM (^. integerValue) y
            & stringTable         .~ mapM (^. stringValue ) y
    )


data RecordData =
  RecordData
  {
    list'  :: Optional 1 (Message RecordList )
  , table' :: Optional 2 (Message RecordTable)
  }
    deriving (Generic, Show)

instance Default RecordData where
  def = RecordData (putField Nothing) (putField Nothing)

instance Decode RecordData

instance Encode RecordData


list :: Lens' RecordData (Maybe RecordList)
list = lens (getField . list') (\s x -> s {list' = putField x})


table :: Lens' RecordData(Maybe RecordTable)
table = lens (getField . table') (\s x -> s {table' = putField x})


recordData :: Lens' RecordData [RecordContent]
recordData =
  lens
    (
      \x ->
        fromMaybe []
           $  (^. recordList ) <$> x ^. list
          <|> (^. recordTable) <$> x ^. table
    )
    (
      \s x ->
        if sameVarTypes $ fmap snd . snd =<< x -- FIXME: Make this more efficient.
          then s & list .~ Nothing                      & table .~ Just (def & recordTable .~ x)
          else s & list .~ Just (def & recordList .~ x) & table .~ Nothing
    )


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
