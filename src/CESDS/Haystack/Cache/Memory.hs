{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module CESDS.Haystack.Cache.Memory (
  CacheM
, runCacheT
, clearCache
, makeCache
, refreshExtractCacheManager
, timeKeys
, cacheSize
) where


import CESDS.Haystack (EpochSeconds, HaystackAccess(..), HaystackTimes(..), Measurement, TimeStamp, haystackHisRead, sEpochSeconds, sMeasurement, sTimeStamp)
import CESDS.Types (Val(..))
import CESDS.Types.Variable (VariableIdentifier)
import Control.Arrow ((&&&))
import Data.Daft.Cache (Cache(..), minimum')
import Data.Daft.Cache.Memory (MemoryCacheT(..))
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone(..))
import Data.Time.Util (SecondsPOSIX, fromSecondsPOSIX)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (rcast)

import qualified Data.HashMap.Strict as H


type CacheM = MemoryCacheT VariableIdentifier (FieldRec '[EpochSeconds]) (FieldRec '[TimeStamp, Measurement]) IO


makeCache :: [(Text, VariableIdentifier)] -> CacheM ()
makeCache = initializeCache . fmap snd


timeKeys :: VariableIdentifier -> CacheM [SecondsPOSIX]
timeKeys = fmap (map (sEpochSeconds <:)) . keysList


cacheSize :: VariableIdentifier -> CacheM Int
cacheSize = keysCount


fromSecondsPOSIX' :: HaystackAccess -> SecondsPOSIX -> String
fromSecondsPOSIX' HaystackAccess{..} =
  let
    (timeZoneMinutes, timeZoneSummerOnly, timeZoneName) = timeZone
  in
    fromSecondsPOSIX TimeZone{..}


refreshExtractCacheManager :: HaystackAccess -> VariableIdentifier -> Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> CacheM [(SecondsPOSIX, Object')]
refreshExtractCacheManager access variable startRequest finishRequest =
  do
    startRequest' <- minimum' startRequest . fmap (sEpochSeconds <:) <$> keysMinimum variable
    if isNothing startRequest
      then return []
      else map asObject <$> lookupRange (fetchHistory access) variable ((sEpochSeconds =:) <$> startRequest') ((sEpochSeconds =:) <$> finishRequest)


fetchHistory :: HaystackAccess -> VariableIdentifier -> Maybe (FieldRec '[EpochSeconds]) -> Maybe (FieldRec '[EpochSeconds]) -> CacheM [(FieldRec '[EpochSeconds], FieldRec '[TimeStamp, Measurement])]
fetchHistory access variable startRequest finishRequest =
  do
    let 
      Just startRequest' = fromSecondsPOSIX' access . (sEpochSeconds <:) <$> startRequest
      finishRequest' = fromSecondsPOSIX' access . (sEpochSeconds <:) <$> finishRequest
    x <- haystackHisRead access variable $ maybe AfterTime (flip TimeRange) finishRequest' startRequest'
    return $ map (rcast &&& rcast) x


type Object' = H.HashMap VariableIdentifier Val


asObject :: (FieldRec '[EpochSeconds], FieldRec '[TimeStamp, Measurement]) -> (Int, Object')
asObject (key, row) =
  (
    sEpochSeconds <: key
  , H.fromList 
    [
      ("time"       ,                            sTimeStamp    <: row)
    , ("epoch"      , Continuous .fromIntegral $ sEpochSeconds <: key)
    , ("measurement",                            sMeasurement  <: row)
    ]
  )
