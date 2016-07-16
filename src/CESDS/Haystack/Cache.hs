{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Haystack.Cache (
  CacheManager(..)
, clearCache
, makeCacheManager
, refreshExtractCacheManager
, refreshCacheManager
, extractForTimes
, timeKeys
, Cache
, newCache
, addHistory
) where


import CESDS.Haystack (HaystackAccess(..), HaystackTimes(..), History, haystackHisRead, sEpochSeconds, sMeasurement, sTimeStamp)
import CESDS.Types (Identifier, Val(Discrete))
import CESDS.Types.Variable (VariableIdentifier)
import Control.Arrow ((&&&), second)
import Control.Monad (foldM, guard)
import Control.Monad.Except (MonadIO)
import Data.Daft.Vinyl.Derived ((<:))
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone(..))
import Data.Time.Util (SecondsPOSIX, fromSecondsPOSIX)

import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as M
import qualified Data.Text as T


data CacheManager =
  CacheManager
  {
    access    :: HaystackAccess
  , entities  :: [Identifier]
  , cache     :: Cache
  , prototype :: Object'
  }
    deriving (Eq, Read, Show)


clearCache :: CacheManager -> CacheManager
clearCache cacheManager =
  cacheManager {cache = M.empty}


makeCacheManager :: MonadIO m => HaystackAccess -> [(Text, Identifier)] -> m CacheManager
makeCacheManager access entities' =
  let
    entities  = map snd entities'
    cache     = newCache
    prototype = H.empty -- FIXME
  in
    return CacheManager{..}


timeKeys :: CacheManager -> [SecondsPOSIX]
timeKeys CacheManager{..} = M.keys cache


extractTimeStamp :: Object' -> String
extractTimeStamp o =
  let
    Discrete s = o H.! "Time Stamp"
  in
    T.unpack s


extractForTimes :: Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> CacheManager -> [(SecondsPOSIX, Object')]
extractForTimes start finish CacheManager{..} =
  M.toList
    . snd 
    . M.mapAccum (((id &&& id) .) . flip updateObject) prototype
    $ subsetByTimes start finish cache


fromSecondsPOSIX' :: HaystackAccess -> SecondsPOSIX -> String
fromSecondsPOSIX' HaystackAccess{..} =
  let
    (timeZoneMinutes, timeZoneSummerOnly, timeZoneName) = timeZone
  in
    fromSecondsPOSIX TimeZone{..}


refreshExtractCacheManager :: MonadIO m => CacheManager -> Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> m (CacheManager, [(SecondsPOSIX, Object')])
refreshExtractCacheManager cacheManager@CacheManager{..} startRequest finishRequest =
  if M.null cache && isNothing startRequest
    then return (cacheManager, [])
    else do
           let
             startRequest' = fromMaybe (fst $ M.findMin cache) startRequest
           cacheManager' <- refreshCacheManager cacheManager startRequest' finishRequest
           return (cacheManager', extractForTimes startRequest finishRequest cacheManager')


refreshCacheManager :: MonadIO m => CacheManager -> SecondsPOSIX -> Maybe SecondsPOSIX -> m CacheManager
refreshCacheManager cacheManager@CacheManager{..} startRequest maybeFinishRequest =
  let
    convert = fromSecondsPOSIX' access
    convert' = second extractTimeStamp
    startRequest' = convert startRequest
    maybeFinishRequest' = convert <$> maybeFinishRequest
    (startCache , startCache' ) = convert' $ M.findMin cache
    (finishCache, finishCache') = convert' $ M.findMax cache
    intervals
      | M.null cache = [maybe AfterTime (flip TimeRange) maybeFinishRequest' startRequest']
      | otherwise    = catMaybes
                       [
                         do
                           guard $ startRequest < startCache
                           return $ TimeRange startRequest' startCache'
                       , do
                           guard $ maybe True (> finishCache) maybeFinishRequest
                           return . maybe AfterTime (flip TimeRange) maybeFinishRequest' $ finishCache'
                       ]
  in
    foldM refreshCacheManager' cacheManager intervals


refreshCacheManager' :: MonadIO m => CacheManager -> HaystackTimes String -> m CacheManager
refreshCacheManager' cacheManager@CacheManager{..} times =
  replaceCache cacheManager
    . foldl addHistory cache
    . zip entities
    <$> mapM (flip (haystackHisRead access) times) entities


replaceCache :: CacheManager -> Cache -> CacheManager
replaceCache cacheManager cache' = cacheManager {cache = cache'}


subsetByTimes :: Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> Cache -> Cache
subsetByTimes Nothing      Nothing       = id
subsetByTimes (Just start) Nothing       = snd . M.split (start - 1)
subsetByTimes Nothing      (Just finish) =                             fst . M.split (finish + 1)
subsetByTimes (Just start) (Just finish) = snd . M.split (start - 1) . fst . M.split (finish + 1)


type Object' = H.HashMap VariableIdentifier Val


type Cache = M.IntMap Object'


newCache :: Cache
newCache = M.empty


asCache :: Identifier -> [History] -> Cache
asCache = (M.fromList .) . map  . asObject


asObject :: Identifier -> History -> (Int, Object')
asObject identifier row =
  (
    sEpochSeconds <: row
  , H.fromList 
    [
      ("Time Stamp", sTimeStamp   <: row)
    , (identifier  , sMeasurement <: row)
    ]
  )


addHistory :: Cache -> (Identifier, [History]) -> Cache
addHistory original (identifier, rows) = asCache identifier rows `updateCache` original


updateCache :: Cache -> Cache -> Cache
updateCache = M.unionWith updateObject


updateObject :: Object' -> Object' -> Object'
updateObject = H.union
