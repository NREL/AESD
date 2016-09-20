{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module CESDS.Haystack.Cache.Memory (
  Cached
, CacheM
, evalCacheT
, execCacheT
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
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO)
import Data.Daft.Cache (Cache(..))
import Data.Daft.Cache.Memory (Container, MemoryCacheT, emptyContainer, evalCacheT, execCacheT, runCacheT)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Maybe (isNothing)
import Data.Text (Text, unpack)
import Data.Time.LocalTime (TimeZone(..))
import Data.Time.Util (SecondsPOSIX, fromSecondsPOSIX)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (rcast)
import Debug.Trace (trace)

import qualified Data.HashMap.Strict as H


type Cached = Container VariableIdentifier (FieldRec '[EpochSeconds]) (FieldRec '[TimeStamp, Measurement])


type CacheM = MemoryCacheT VariableIdentifier (FieldRec '[EpochSeconds]) (FieldRec '[TimeStamp, Measurement])


makeCache :: [(Text, VariableIdentifier)] -> Cached
makeCache = emptyContainer . fmap snd


timeKeys :: MonadError String m => VariableIdentifier -> CacheM m [SecondsPOSIX]
timeKeys = fmap (map (sEpochSeconds <:)) . keysList


cacheSize :: MonadError String m => VariableIdentifier -> CacheM m Int
cacheSize = keysCount


fromSecondsPOSIX' :: HaystackAccess -> SecondsPOSIX -> String
fromSecondsPOSIX' HaystackAccess{..} =
  let
    (timeZoneMinutes, timeZoneSummerOnly, timeZoneName) = timeZone
  in
    fromSecondsPOSIX TimeZone{..}


refreshExtractCacheManager :: (MonadIO m, MonadError String m) => HaystackAccess -> VariableIdentifier -> Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> CacheM m [(SecondsPOSIX, Object')]
refreshExtractCacheManager access variable startRequest finishRequest =
  do
    startCache <- fmap (sEpochSeconds <:) <$> keysMinimum variable
    let
      startRequest' = startRequest <|> startCache
    if isNothing startRequest'
      then return []
      else map asObject
             <$> lookupRange
                 (fetchHistory access)
                 variable
                 ((sEpochSeconds =:) <$> startRequest')
                 ((sEpochSeconds =:) <$> finishRequest)


fetchHistory :: MonadIO m => HaystackAccess -> VariableIdentifier -> Maybe (FieldRec '[EpochSeconds]) -> Maybe (FieldRec '[EpochSeconds]) -> CacheM m [(FieldRec '[EpochSeconds], FieldRec '[TimeStamp, Measurement])]
fetchHistory access variable startRequest finishRequest =
  do
    let 
      Just startRequest' = fromSecondsPOSIX' access . (sEpochSeconds <:) <$> startRequest
      finishRequest' = fromSecondsPOSIX' access . (sEpochSeconds <:) <$> finishRequest
    x <- haystackHisRead access variable $ maybe AfterTime (flip TimeRange) finishRequest' startRequest'
    return
      . trace ("Reading sensor " ++ unpack variable ++ " from " ++ show startRequest ++ " to " ++ show finishRequest ++ ".")
      $ map (rcast &&& rcast) x


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
