{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module CESDS.Haystack.Cache.Singular (
  CacheManager(..)
, Cache
, clearCache
, clearSubcache
, makeCacheManager
, refreshExtractCacheManager
, timeKeys
, cacheSize
) where


import CESDS.Haystack (EpochSeconds, HaystackAccess(..), HaystackTimes(..), History, Measurement, TimeStamp, haystackHisRead, sEpochSeconds, sMeasurement, sTimeStamp)
import CESDS.Types (Val(..))
import CESDS.Types.Variable (VariableIdentifier)
import Control.Arrow ((***))
import Control.Monad (guard)
import Control.Monad.Except (MonadIO)
import Data.Daft.DataCube
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Daft.Vinyl.FieldCube
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone(..))
import Data.Time.Util (SecondsPOSIX, fromSecondsPOSIX)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T


data CacheManager =
  CacheManager
  {
    access :: HaystackAccess
  , cache  :: Cache
  }


type Cache = H.HashMap VariableIdentifier Subcache


type Subcache = FieldCube '[EpochSeconds] '[TimeStamp, Measurement]


clearCache :: CacheManager -> CacheManager
clearCache cacheManager@CacheManager{..} =
  cacheManager
  {
    cache = const mempty <$> cache
  }


clearSubcache :: VariableIdentifier -> CacheManager -> CacheManager
clearSubcache variable cacheManager@CacheManager{..} =
  cacheManager
  {
    cache = H.insert variable mempty cache
  }


makeCacheManager :: MonadIO m => HaystackAccess -> [(Text, VariableIdentifier)] -> m CacheManager
makeCacheManager access variables =
  let
    cache =
      H.fromList
        $ map ((, mempty) . snd) variables
  in
    return CacheManager{..}


timeKeys :: VariableIdentifier -> CacheManager -> [SecondsPOSIX]
timeKeys variable CacheManager{..} =
  maybe [] (projectKnownKeys (sEpochSeconds <:))
    $ variable `H.lookup` cache


cacheSize :: VariableIdentifier -> CacheManager -> Int
cacheSize variable CacheManager{..} =
  maybe 0 knownSize
    $ variable `H.lookup` cache


extractTimeStamp :: Val -> String
extractTimeStamp v =
  let
    Discrete s = v
  in
    T.unpack s


extractForTimes :: Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> Subcache -> [(SecondsPOSIX, Object')]
extractForTimes start finish =
  map asObject
    . toKnownRecords
    . selectRange
      ((sEpochSeconds =:) . (+ (-1)) <$> start )
      ((sEpochSeconds =:) . (+   1 ) <$> finish)


fromSecondsPOSIX' :: HaystackAccess -> SecondsPOSIX -> String
fromSecondsPOSIX' HaystackAccess{..} =
  let
    (timeZoneMinutes, timeZoneSummerOnly, timeZoneName) = timeZone
  in
    fromSecondsPOSIX TimeZone{..}


refreshExtractCacheManager :: MonadIO m => CacheManager -> VariableIdentifier -> Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> m (CacheManager, [(SecondsPOSIX, Object')])
refreshExtractCacheManager cacheManager variable startRequest finishRequest =
  let
    subcache = cache cacheManager H.! variable
  in
    if knownEmpty subcache && isNothing startRequest
      then return (cacheManager, [])
      else do
             let
               startRequest' = fromMaybe ((sEpochSeconds <:) . fst . fromJust $ selectKnownMinimum subcache) startRequest
             cacheManager' <- refreshCacheManager cacheManager variable startRequest' finishRequest
             let
               subcache' = cache cacheManager' H.! variable
             return (cacheManager', extractForTimes startRequest finishRequest subcache')


refreshCacheManager :: MonadIO m => CacheManager -> VariableIdentifier -> SecondsPOSIX -> Maybe SecondsPOSIX -> m CacheManager
refreshCacheManager cacheManager@CacheManager{..} variable startRequest maybeFinishRequest =
  do
    let
      subcache = cache H.! variable
      convert = fromSecondsPOSIX' access
      convert' = (sEpochSeconds <:) *** (extractTimeStamp . (sTimeStamp <:))
      startRequest'       = convert     startRequest
      maybeFinishRequest' = convert <$> maybeFinishRequest
      (startCache , startCache' ) = convert' . fromJust $ selectKnownMinimum subcache
      (finishCache, finishCache') = convert' . fromJust $ selectKnownMaximum subcache
      intervals
        | knownEmpty subcache = [maybe AfterTime (flip TimeRange) maybeFinishRequest' startRequest']
        | otherwise           = catMaybes
                                [
                                  do
                                    guard $ startRequest < startCache
                                    return $ TimeRange startRequest' startCache'
                                , do
                                    guard $ maybe True (> finishCache) maybeFinishRequest
                                    return . maybe AfterTime (flip TimeRange) maybeFinishRequest' $ finishCache'
                                ]
    newHistory <- concat <$> mapM (haystackHisRead access variable) intervals
    return
      $ cacheManager
        {
          cache = H.insert variable (fromRecords newHistory `mappend` subcache) cache
        }


type Object' = H.HashMap VariableIdentifier Val


asObject :: History -> (Int, Object')
asObject row =
  (
    sEpochSeconds <: row
  , H.fromList 
    [
      ("time"       ,                            sTimeStamp    <: row)
    , ("epoch"      , Continuous .fromIntegral $ sEpochSeconds <: row)
    , ("measurement",                            sMeasurement  <: row)
    ]
  )
