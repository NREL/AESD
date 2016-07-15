{-# LANGUAGE OverloadedStrings #-}


module CESDS.Haystack.Cache (
  Cache
, newCache
, addHistory
) where


import CESDS.Haystack (History, sEpochSeconds, sMeasurement, sTimeStamp)
import CESDS.Types.Variable (VariableIdentifier)
import Data.Aeson.Types (Object, (.=))
import Data.Daft.Vinyl.Derived ((<:))

import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as M


type Cache = M.IntMap Object


newCache :: Cache
newCache = M.empty


asCache :: VariableIdentifier -> [History] -> Cache
asCache = (M.fromList .) . map  . asObject


asObject :: VariableIdentifier -> History -> (Int, Object)
asObject identifier row =
  (
    sEpochSeconds <: row
  , H.fromList 
    [
      "Time Stamp" .= sTimeStamp   <: row
    , identifier   .= sMeasurement <: row
    ]
  )


addHistory :: Cache -> (VariableIdentifier, [History]) -> Cache
addHistory original (identifier, rows) = asCache identifier rows `updateCache` original


updateCache :: Cache -> Cache -> Cache
updateCache = M.unionWith updateObject


updateObject :: Object -> Object -> Object
updateObject = H.union
