{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Haystack.Cache (
  CacheManager(..)
, makeCacheManager
, refreshCacheManager
, extractForTimes
, Cache
, newCache
, addHistory
) where


import CESDS.Haystack (HaystackAccess(..), HaystackTimes(..), History, haystackHisRead, sEpochSeconds, sMeasurement, sTimeStamp)
import CESDS.Types (Identifier)
import Control.Arrow ((&&&), second)
import Control.Monad.Except (MonadIO)
import Data.Aeson.Types (Object, Value(Null, String), (.=))
import Data.Daft.Vinyl.Derived ((<:))
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone(..))
import Data.Time.Util (SecondsPOSIX, fromSecondsPOSIX)
import Debug.Trace (trace)

import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as M
import qualified Data.Text as T


data CacheManager =
  CacheManager
  {
    access    :: HaystackAccess
  , entities  :: [Identifier]
  , cache     :: Cache
  , prototype :: Object
  }
    deriving (Eq, Read, Show)


makeCacheManager :: MonadIO m => HaystackAccess -> [(Text, Identifier)] -> m CacheManager
makeCacheManager access entities' =
  let
    entities  = map snd entities'
    cache     = newCache
    prototype = H.fromList $ map (.= Null) entities
  in
    refreshCacheManager' CacheManager{..} Today


extractTimeStamp :: Object -> String
extractTimeStamp o =
  let
    String s = o H.! "Time Stamp"
  in
    T.unpack s


extractForTimes :: Maybe SecondsPOSIX -> Maybe SecondsPOSIX -> CacheManager -> [Object]
extractForTimes start finish CacheManager{..} =
  M.elems
    . snd 
    . M.mapAccum (((id &&& id) .) . flip updateObject) prototype
    $ subsetByTimes start finish cache


fromSecondsPOSIX' :: HaystackAccess -> SecondsPOSIX -> String
fromSecondsPOSIX' HaystackAccess{..} =
  let
    (timeZoneMinutes, timeZoneSummerOnly, timeZoneName) = timeZone
  in
    fromSecondsPOSIX TimeZone{..}


refreshCacheManager :: MonadIO m => CacheManager -> SecondsPOSIX -> m CacheManager
refreshCacheManager cacheManager@CacheManager{..} startTime = -- FIXME: Sloppy.
  do
    let
      cacheMin = second extractTimeStamp       $ M.findMin cache
      cacheMax =        extractTimeStamp . snd $ M.findMax cache
    cacheManager' <- refreshCacheManager' cacheManager $ AfterTime cacheMax
    if startTime >= fst cacheMin
      then trace "HERE" $ return cacheManager'
      else
        trace "THERE" $ refreshCacheManager' cacheManager'
          $ TimeRange (fromSecondsPOSIX' access startTime) (snd cacheMin)


refreshCacheManager' :: MonadIO m => CacheManager -> HaystackTimes String -> m CacheManager
refreshCacheManager' cacheManager@CacheManager{..} times =
  trace (show times) . replaceCache cacheManager
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


type Cache = M.IntMap Object


newCache :: Cache
newCache = M.empty


asCache :: Identifier -> [History] -> Cache
asCache = (M.fromList .) . map  . asObject


asObject :: Identifier -> History -> (Int, Object)
asObject identifier row =
  (
    sEpochSeconds <: row
  , H.fromList 
    [
      "Time Stamp" .= sTimeStamp   <: row
    , identifier   .= sMeasurement <: row
    ]
  )


addHistory :: Cache -> (Identifier, [History]) -> Cache
addHistory original (identifier, rows) = asCache identifier rows `updateCache` original


updateCache :: Cache -> Cache -> Cache
updateCache = M.unionWith updateObject


updateObject :: Object -> Object -> Object
updateObject = H.union
