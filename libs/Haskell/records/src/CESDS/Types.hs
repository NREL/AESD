{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for the CESDS API.
-}


module CESDS.Types (
-- * Versioning
  VersionIdentifier
, currentVersion
) where


import Data.Word (Word32)


-- | A version of the CESDS API.
type VersionIdentifier = Word32


-- | The current version of the CESDS API.
currentVersion :: VersionIdentifier
currentVersion = 4
