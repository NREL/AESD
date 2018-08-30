{-|
Module      :  $Header$
Copyright   :  (c) 2016-18 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Types for the AESD API.
-}


module AESD.Types (
-- * Versioning
  VersionIdentifier
, currentVersion
) where


import Data.Word (Word32)


-- | A version of the AESD API.
type VersionIdentifier = Word32


-- | The current version of the AESD API.
currentVersion :: VersionIdentifier
currentVersion = 4
