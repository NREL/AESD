{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module CESDS.Haystack.Cache.Memory (
  Cached
, CacheM
, runCacheT
, clearCache
, makeCache
, refreshExtractCacheManager
, timeKeys
, cacheSize
) where


import CESDS.Haystack (EpochSeconds, HaystackAccess(..), HaystackTimes(..), Measurement, MeasurementIdentifier, TimeStamp, haystackHisRead, sEpochSeconds, sMeasurement, sTimeStamp)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Value (integerValue, realValue, stringValue)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO)
import Data.Daft.Cache (Cache(..))
import Data.Daft.Cache.Memory (Container, MemoryCacheT, emptyContainer, runCacheT)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Default (def)
import Data.Maybe (isNothing)
import Data.Text (Text, unpack)
import Data.Time.LocalTime (TimeZone(..))
import Data.Time.Util (SecondsPOSIX, fromSecondsPOSIX)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (rcast)
import Debug.Trace (trace)
import Network.HTTP.Conduit (Manager)


type Cached = Container MeasurementIdentifier (FieldRec '[EpochSeconds]) (FieldRec '[TimeStamp, Measurement])


type CacheM = MemoryCacheT MeasurementIdentifier (FieldRec '[EpochSeconds]) (FieldRec '[TimeStamp, Measurement])


makeCache :: [(Text, MeasurementIdentifier)] -> Cached
makeCache = emptyContainer . fmap snd


timeKeys :: MonadError String m => MeasurementIdentifier -> CacheM m [SecondsPOSIX]
timeKeys = fmap (map (sEpochSeconds <:)) . keysList


cacheSize :: MonadError String m => MeasurementIdentifier -> CacheM m Int
cacheSize = keysCount


fromSecondsPOSIX' :: HaystackAccess -> SecondsPOSIX -> String
fromSecondsPOSIX' HaystackAccess{..} =
  let
    (timeZoneMinutes, timeZoneSummerOnly, timeZoneName) = timeZone
  in
    fromSecondsPOSIX TimeZone{..}


refreshExtractCacheManager :: (MonadIO m, MonadError String m) => Manager -> HaystackAccess -> MeasurementIdentifier -> Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> CacheM m [RecordContent]
refreshExtractCacheManager manager access variable startRequest finishRequest =
  do
    startCache <- fmap (sEpochSeconds <:) <$> keysMinimum variable
    let
      startRequest' = startRequest <|> startCache
    if isNothing startRequest'
      then return []
      else map asObject
             <$> lookupRange
                 (fetchHistory manager access)
                 variable
                 ((sEpochSeconds =:) <$> startRequest')
                 ((sEpochSeconds =:) <$> finishRequest)


fetchHistory :: MonadIO m => Manager -> HaystackAccess -> MeasurementIdentifier -> Maybe (FieldRec '[EpochSeconds]) -> Maybe (FieldRec '[EpochSeconds]) -> CacheM m [(FieldRec '[EpochSeconds], FieldRec '[TimeStamp, Measurement])]
fetchHistory manager access variable startRequest finishRequest =
  do
    let 
      Just startRequest' = fromSecondsPOSIX' access . (sEpochSeconds <:) <$> startRequest
      finishRequest' = fromSecondsPOSIX' access . (sEpochSeconds <:) <$> finishRequest
    x <- haystackHisRead manager access variable $ maybe AfterTime (flip TimeRange) finishRequest' startRequest'
    return
      . trace ("Reading sensor " ++ unpack variable ++ " from " ++ show startRequest ++ " to " ++ show finishRequest ++ ".")
      $ map (rcast &&& rcast) x


asObject :: (FieldRec '[EpochSeconds], FieldRec '[TimeStamp, Measurement]) -> RecordContent
asObject (key, row) =
  (
    fromIntegral $ sEpochSeconds <: key
  , [
      (0, def & stringValue  .~ Just (               sTimeStamp    <: row))
    , (1, def & integerValue .~ Just (fromIntegral $ sEpochSeconds <: key))
    , (2, def & realValue    .~ Just (               sMeasurement  <: row))
    ]
  )
