{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Support for file-based serving of records.
-}


module AESD.Records.Server.File (
-- * Metadata
  buildVarMetas
, buildVarMeta
-- * Data
, buildModelContent
) where


import AESD.Types.Record (RecordContent)
import AESD.Types.Value (VarType(..), castVarType, commonVarTypes)
import AESD.Types.Variable (VariableIdentifier, VarMeta, VarUnits(unitName), makeVarMeta, units, varType)
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Default (def)
import Text.Regex.Posix ((=~))


{-# ANN module "HLint: ignore Reduce duplication" #-}


-- | Construct variable metadata for a table of data.  The strings in the table are analyzed to determine whether each column is a real, integer, or string value.  If the name of the variable contains its units in square brackets, the units are separated from the variable and put in the units metadata.
buildVarMetas :: [[String]] -- ^ The rows of data.
              -> [VarMeta]  -- ^ The variable metadata for each column of the table.
buildVarMetas (header : content) =
  let
    values = fmap read <$> content
    types = commonVarTypes values
    vids = [0..]
  in
    zipWith3 buildVarMeta vids header types
buildVarMetas [] = def


-- | Construct Variable metadata.  If the name of the variable contains its units in square brackets, the units are separated from the variable and put in the units metadata.
buildVarMeta :: VariableIdentifier -- ^ The variable identifier.
             -> String             -- ^ The name of the variable.
             -> VarType            -- ^ The type for the variable.
             -> VarMeta            -- ^ The variable metadata.
buildVarMeta i n t =
  let
    (n', u) = case n =~ "^(.*[^ ]) *[[](.*)[]] *$" :: [[String]] of
                [[_, n'', u'']] -> (n'', u'')
                _               -> (n, "unknown")
  in
    makeVarMeta i n'
      & varType    .~ t
      & units      .~ def {unitName = Just u}
  

-- | Construct records from a table of data.  The strings in the table are parsed into values with the most specific value type in each column.
buildModelContent :: [[String]]      -- ^ The rows of data.
                  -> [RecordContent] -- ^ The records.
buildModelContent (_ : content) =
  let
    values = fmap read <$> content
    types = commonVarTypes values
    vids = [0..]
    rids = [1..]
  in
    zipWith (\rid vs -> (rid, zip vids vs)) rids
      $ zipWith castVarType types
      <$> values
buildModelContent [] = []
