{-|
Module      :  $Header$
Copyright   :  (c) 2016-18 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Access to Haystack servers.
-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module AESD.Haystack (
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
, MeasurementIdentifier
, EpochSeconds
, sEpochSeconds
, TimeStamp
, sTimeStamp
, Measurement
, sMeasurement
) where


import Control.Arrow ((***))
import Control.Exception (throwIO)
import Control.Monad.Except (MonadIO, liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value, (.=), object)
import Data.Aeson.Util (extract)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Util (toSecondsPOSIX)
import Data.Daft.Vinyl.FieldRec ((=:), (<+>))
import Data.Vinyl.Derived (FieldRec, SField(..))
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Manager, Response, httpLbs)
import Network.HTTP.Simple (Request, addRequestHeader, defaultRequest, getResponseBody, setRequestBasicAuth, setRequestHost, setRequestPath, setRequestPort, setRequestQueryString, setRequestSecure)

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Text as T (drop, pack, unpack)


type MeasurementIdentifier = Text


data HaystackAccess =
  HaystackAccess
  {
    server        :: String
  , root          :: String
  , authorization :: Maybe (String, String)
  , secure        :: Maybe Bool
  , port          :: Maybe Int
  , timeZone      :: (Int, Bool, String)
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


setNavId :: Maybe MeasurementIdentifier -> Request -> Request
setNavId Nothing           = id
setNavId (Just identifier) = setRequestQueryString [("navId", Just . BS.pack $ T.unpack identifier)]


setId :: MeasurementIdentifier -> Request -> Request
setId identifier = setRequestQueryString [("id", Just . BS.pack $ T.unpack identifier)]


haystackNav :: MonadIO m => Manager -> HaystackAccess -> Maybe MeasurementIdentifier -> m [Value]
haystackNav manager access identifier =
  let
    request =
        addRequestHeader "Accept" "application/json"
      $ setNavId identifier
      $ haystackRequest access "/nav"
  in
    extract "rows"
      . getResponseBody
      <$> httpJSON manager request


haystackNavTree :: MonadIO m => Manager -> HaystackAccess -> Maybe MeasurementIdentifier -> m Value
haystackNavTree manager access identifier =
  let
    visit parent =
      do
        let navId = getNavId parent
        children <- haystackNavTree manager access $ Just navId
        return $ object ["entity" .= parent, "children" .= children]
  in
    fmap toJSON
      . mapM visit
      =<< haystackNav manager access identifier


haystackRead :: MonadIO m => Manager -> HaystackAccess -> MeasurementIdentifier -> m Value
haystackRead manager access identifier =
  let
    request =
        addRequestHeader "Accept" "application/json"
      $ setId identifier
      $ haystackRequest access "/read"
  in
    getResponseBody
      <$> httpJSON manager request


httpJSON :: (MonadIO m, FromJSON a) => Manager -> Request -> m (Response a)
httpJSON manager request =
  do
    response <- httpLbs request manager
    case sequence $ eitherDecode <$> response of
      Left  message -> liftIO . throwIO $ userError message
      Right result  -> return result


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


type EpochSeconds = '("Epoch Seconds", Int)


type TimeStamp    = '("Time Stamp"   , String)


type Measurement  = '("Measurement"  , Double)


sEpochSeconds :: SField EpochSeconds
sEpochSeconds = SField


sTimeStamp :: SField TimeStamp
sTimeStamp = SField


sMeasurement :: SField Measurement
sMeasurement = SField


haystackHisRead :: MonadIO m => Manager -> HaystackAccess -> MeasurementIdentifier -> HaystackTimes String -> m [History]
haystackHisRead manager access identifier times =
  let
    request =
        addRequestHeader "Accept" "application/json"
      $ setTimes identifier times
      $ haystackRequest access "/hisRead"
  in
    extractHistory
      . getResponseBody
      <$> httpJSON manager request


setTimes :: MeasurementIdentifier -> HaystackTimes String -> Request -> Request
setTimes identifier times =
  setRequestQueryString
    [
      ("id"   , Just . BS.pack $ T.unpack identifier    )
    , ("range", Just . BS.pack $ showHaystackTimes times)
    ]


extractHistory :: Value -> [History]
extractHistory o =
  [
        sEpochSeconds =: fromIntegral (toSecondsPOSIX $ T.unpack ts)
    <+> sTimeStamp    =: T.unpack ts
    <+> sMeasurement  =: val
  |
    o' <- extract "rows" o
  , let ts  =                           T.drop 2 $ extract "ts" o'
        val = read . takeWhile (/= ' ') . drop 2 $ extract "val" o'
  ]
   
 
getNavId :: Value -> MeasurementIdentifier
getNavId = T.pack . getIdField "navId"


getId :: Value -> MeasurementIdentifier
getId = T.pack . getIdField "id"


getIdField :: String -> Value -> String
getIdField label o =
  case extract label o of
    'u' : ':' : x -> "`" ++ x ++ "`"
    'r' : ':' : x -> "@" ++ x
    x             -> x
