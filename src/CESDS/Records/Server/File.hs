module CESDS.Records.Server.File (
  buildModelMeta
, buildModelContent
) where


import CESDS.Types.Model (ModelMeta, varMeta)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Value (VarType(..), castVarType, consistentVarTypes)
import CESDS.Types.Variable (identifier, name, varType)
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Default (def)


buildModelMeta :: [[String]] -> ModelMeta
buildModelMeta (header : content) =
  let
    values = fmap read <$> content
    types = foldl consistentVarTypes (const IntegerVar <$> header) values
    vids = [0..]
  in
    def
      & varMeta .~ zipWith3 (\vid n t -> def & identifier .~ vid & name .~ n & varType .~ t) vids header types
buildModelMeta [] = def


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
