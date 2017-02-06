module CESDS.Records.Server.File (
  buildModelMeta
, buildVarMeta
, buildModelContent
) where


import CESDS.Types.Model (ModelMeta, varMeta)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Value (VarType(..), castVarType, consistentVarTypes)
import CESDS.Types.Variable (VarMeta, VarUnits(unitName), identifier, name, units, varType)
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Default (def)
import Data.Int (Int32)
import Text.Regex.Posix ((=~))


buildModelMeta :: [[String]] -> ModelMeta
buildModelMeta (header : content) =
  let
    values = fmap read <$> content
    types = foldl consistentVarTypes (const IntegerVar <$> header) values
    vids = [0..]
  in
    def
      & varMeta .~ zipWith3 buildVarMeta vids header types
buildModelMeta [] = def


buildVarMeta :: Int32 -> String -> VarType -> VarMeta
buildVarMeta i n t =
  let
    (n', u) = case n =~ "^(.*[^ ]) *[[](.*)[]] *$" :: [[String]] of
                [[_, n'', u'']] -> (n'', u'')
                _               -> (n, "unknown")
  in
    def
      & identifier .~ i
      & name       .~ n'
      & varType             .~ t
      & units               .~ def {unitName = Just u}
  

buildModelContent :: [[String]] -> [RecordContent]
buildModelContent (header : content) =
  let
    values = fmap read <$> content
    types = foldl consistentVarTypes (const IntegerVar <$> header) values
    vids = [0..]
    rids = [0..]
  in
    zipWith (\rid vs -> (rid, zip vids vs)) rids
      $ zipWith castVarType types
      <$> values
buildModelContent [] = []
