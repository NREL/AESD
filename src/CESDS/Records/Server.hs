module CESDS.Records.Server (
  buildModel
) where


import CESDS.Types.Model (ModelMeta, varMeta)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Value (VarType(..), castVarType, consistentVarTypes)
import CESDS.Types.Variable (identifier, name, varType)
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Data.Default (def)


buildModel :: [[String]] -> (ModelMeta, [RecordContent])
buildModel (header : content) =
  let
    values = fmap read <$> content
    types = foldl consistentVarTypes (const IntegerVar <$> header) values
    vids = [1..]
    rids = [1..]
  in
    (
      def
        & varMeta .~ zipWith3 (\vid n t -> def & identifier .~ vid & name .~ n & varType .~ t) vids header types
    , zipWith (\rid vs -> (rid, zip vids vs)) rids
        $ zipWith castVarType types
        <$> values
    )
