{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Haystack (
  HaystackAccess(..)
, HaystackTimes(..)
, haystackRequest
, haystackNav
, haystackNavTree
, haystackRead
, haystackHisRead
, getNavId
, getId
, History
, EpochSeconds
, sEpochSeconds
, TimeStamp
, sTimeStamp
, Measurement
, sMeasurement
) where


import CESDS.Types (Identifier, Val(..))
import Control.Arrow ((***))
import Control.Monad.Except (MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value, (.=), object)
import Data.Aeson.Util (extract)
import Data.Maybe (fromMaybe)
import Data.Time.Util (toSecondsPOSIX)
import Data.Vinyl ((<+>))
import Data.Vinyl.Derived (FieldRec, SField(..), (=:))
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, addRequestHeader, defaultRequest, getResponseBody, httpJSON, setRequestBasicAuth, setRequestHost, setRequestPath, setRequestPort, setRequestQueryString, setRequestSecure)

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Text as T (drop, pack, unpack)


data HaystackAccess =
  HaystackAccess
  {
    server        :: String
  , root          :: String
  , authorization :: Maybe (String, String)
  , secure        :: Maybe Bool
  , port          :: Maybe Int
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON HaystackAccess where

instance ToJSON HaystackAccess where


haystackRequest :: HaystackAccess -> String -> Request
haystackRequest HaystackAccess{..} path =
    setRequestHost (BS.pack server)
  $ setRequestPath (BS.pack $ root ++ path)
  $ setRequestSecure secure'
  $ setRequestPort (fromMaybe (if secure' then 443 else 80) port)
  $ maybe id (uncurry setRequestBasicAuth . (BS.pack *** BS.pack)) authorization
    defaultRequest
    where
      secure' = fromMaybe True secure


setNavId :: Maybe Identifier -> Request -> Request
setNavId Nothing           = id
setNavId (Just identifier) = setRequestQueryString [("navId", Just . BS.pack $ T.unpack identifier)]


setId :: Identifier -> Request -> Request
setId identifier = setRequestQueryString [("id", Just . BS.pack $ T.unpack identifier)]


haystackNav :: MonadIO m => HaystackAccess -> Maybe Identifier -> m [Value]
haystackNav access identifier =
  let
    request =
        addRequestHeader "Accept" "application/json"
      $ setNavId identifier
      $ haystackRequest access "/nav"
  in
    extract "rows"
      . getResponseBody
      <$> httpJSON request


haystackNavTree :: MonadIO m => HaystackAccess -> Maybe Identifier -> m Value
haystackNavTree access identifier =
  let
    visit parent =
      do
        let navId = getNavId parent
        children <- haystackNavTree access . trace (T.unpack $ getId parent) $ Just navId
        return $ object ["entity" .= parent, "children" .= children]
  in
    fmap toJSON
      . mapM visit
      =<< haystackNav access identifier


haystackRead :: MonadIO m => HaystackAccess -> Identifier -> m Value
haystackRead access identifier =
  let
    request =
        addRequestHeader "Accept" "application/json"
      $ setId identifier
      $ haystackRequest access "/read"
  in
    getResponseBody
      <$> httpJSON request


data HaystackTimes a =
    Today
  | Yesterday
  | Date
    {
      startDate :: a
    }
  | DateRange
    {
      startDate :: a
    , endDate   :: a
    }
  | TimeRange
    {
      startTime :: a
    , endTime   :: a
    }
  | AfterTime
    {
      startTime :: a
    }
    deriving (Eq, Generic, Read, Show)

instance FromJSON a => FromJSON (HaystackTimes a) where

instance ToJSON a => ToJSON (HaystackTimes a) where


showHaystackTimes :: HaystackTimes String -> String
showHaystackTimes Today         = "today"
showHaystackTimes Yesterday     = "yesterday"
showHaystackTimes Date{..}      = show   startDate
showHaystackTimes DateRange{..} = show $ startDate ++ "," ++ endDate
showHaystackTimes TimeRange{..} = show $ startTime ++ "," ++ endTime
showHaystackTimes AfterTime{..} = show   startTime


type History = FieldRec '[EpochSeconds, TimeStamp, Measurement]

type EpochSeconds = '("Epoch Seconds", Integer)

type TimeStamp    = '("Time Stamp"   , Val    )

type Measurement  = '("Measurement"  , Val    )

sEpochSeconds :: SField EpochSeconds
sEpochSeconds = SField

sTimeStamp :: SField TimeStamp
sTimeStamp = SField

sMeasurement :: SField Measurement
sMeasurement = SField


haystackHisRead :: MonadIO m => HaystackAccess -> Identifier -> HaystackTimes String -> m [History]
haystackHisRead access identifier times =
  let
    request =
        addRequestHeader "Accept" "application/json"
      $ setTimes identifier times
      $ haystackRequest access "/hisRead"
  in
    extractHistory
      . getResponseBody
      <$> httpJSON request


setTimes :: Identifier -> HaystackTimes String -> Request -> Request
setTimes identifier times =
  setRequestQueryString
    [
      ("id"   , Just . BS.pack $ T.unpack identifier    )
    , ("range", Just . BS.pack $ showHaystackTimes times)
    ]


extractHistory :: Value -> [History]
extractHistory o =
  [
        sEpochSeconds =: toSecondsPOSIX (T.unpack ts)
    <+> sTimeStamp    =: Discrete ts
    <+> sMeasurement  =: Continuous val
  |
    o' <- extract "rows" o
  , let ts  =                           T.drop 2 $ extract "ts" o'
        val = read . takeWhile (/= ' ') . drop 2 $ extract "val" o'
  ]
   
 
getNavId :: Value -> Identifier
getNavId = T.pack . getIdField "navId"


getId :: Value -> Identifier
getId = T.pack . getIdField "id"


getIdField :: String -> Value -> String
getIdField label o =
  case extract label o of
    'u' : ':' : x -> "`" ++ x ++ "`"
    'r' : ':' : x -> "@" ++ x
    x             -> x
