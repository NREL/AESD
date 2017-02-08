{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}


module CESDS.Records.Server.HDBC (
  hdbcMain
, buildModelMeta
, buildModelContent
, analyzeColumn
) where


import CESDS.Records.Server (serverMain)
import CESDS.Records.Server.File (buildVarMeta)
import CESDS.Records.Server.Manager (makeInMemoryManager)
import CESDS.Types.Model (ModelMeta, varMeta)
import CESDS.Types.Model as Model (identifier, name, uri)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Value (DataValue, VarType(..), integerValue, realValue, stringValue)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (void)
import Data.Default (def)
import Data.List (isSuffixOf)
import Data.UUID (toString)
import Data.UUID.V5 (generateNamed, namespaceURL)
import Database.HDBC (IConnection, describeResult, execute, fetchAllRows, fetchRowAL, prepare)
import Database.HDBC.ColTypes (SqlColDesc(..), SqlTypeId(..))
import Database.HDBC.SqlValue (SqlValue(..), fromSql)
import Database.HDBC.Statement (Statement(originalQuery))
import System.Directory (getDirectoryContents, makeAbsolute)
import System.FilePath.Posix ((</>))


buildModelMeta :: IConnection c => Bool -> c -> String -> IO ModelMeta
buildModelMeta useDescribe connection query =
  do
    statement <- prepare connection $ "SELECT * FROM (" ++ query ++ ") AS buildModelMeta LIMIT " ++ (if useDescribe then "0" else "1")
    void $ execute statement []
    namesTypes <- analyzeColumns useDescribe connection statement
    let
      vids = [1..]
    return
      $ def
      & varMeta
      .~ zipWith (\i (n, t, _) -> buildVarMeta i n t) vids namesTypes


buildModelContent :: IConnection c => Bool -> c -> String -> IO [RecordContent]
buildModelContent useDescribe connection query =
  do
    statement <- prepare connection query
    void $ execute statement []
    (_, _, casts) <- unzip3 <$> analyzeColumns useDescribe connection statement
    values <- fetchAllRows statement
    let
      vids = [1..]
      rids = [1..]
    return
      $ zipWith (\rid vs -> (rid, zip vids vs)) rids
      $ zipWith ($) casts
      <$> values


analyzeColumns :: IConnection c => Bool -> c -> Statement -> IO [(String, VarType, SqlValue -> DataValue)]
analyzeColumns True _ statement = fmap analyzeColumn <$> describeResult statement
analyzeColumns False connection statement =
  do
    statement' <- prepare connection $ originalQuery statement
    void $ execute statement' []
    maybe [] (fmap analyzeColumn') <$> fetchRowAL statement'


analyzeColumn :: (String, SqlColDesc) -> (String, VarType, SqlValue -> DataValue)
analyzeColumn (column, SqlColDesc{..}) = analyzeType column colType


analyzeColumn' :: (String, SqlValue) -> (String, VarType, SqlValue -> DataValue)
analyzeColumn' (column, SqlWord32   _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlWord64   _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlInt32    _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlInt64    _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlInteger  _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlDouble   _) = analyzeType column SqlDoubleT 
analyzeColumn' (column, SqlRational _) = analyzeType column SqlDoubleT 
analyzeColumn' (column, _            ) = analyzeType column SqlVarCharT


analyzeType :: String -> SqlTypeId -> (String, VarType, SqlValue -> DataValue)
analyzeType column typ
  | typ `elem` [SqlNumericT, SqlSmallIntT, SqlIntegerT, SqlTinyIntT, SqlBigIntT] = (column, IntegerVar, flip (integerValue .~) def . Just . fromSql')
  | typ `elem` [SqlDecimalT, SqlRealT, SqlFloatT, SqlDoubleT]                    = (column, RealVar   , flip (realValue    .~) def . Just . fromSql')
  | otherwise                                                                    = (column, StringVar , flip (stringValue  .~) def . Just . fromSql')
  where
    fromSql' SqlNull = def
    fromSql' x       = fromSql x


hdbcMain :: IConnection c => Bool -> String -> Int -> FilePath -> Maybe FilePath -> c -> IO ()
hdbcMain useDescribe host port directory persistence connection =
  do
    directory' <- makeAbsolute directory
    files <- getDirectoryContents directory'
    serverMain host port
      =<< makeInMemoryManager persistence ()
      (
        const
          . fmap (, ())
          $ sequence
          [
            (Model.identifier .~ toString (generateNamed namespaceURL $ toEnum . fromEnum <$> "file://" ++ file))
              . (Model.name .~ file)
              . (uri .~ "file://" ++ file)
              <$> (buildModelMeta useDescribe connection =<< readFile file)
          |
            file <- (directory' </>) <$> files
          , ".sql" `isSuffixOf` file
          ]
      )
      ( \_ m ->
        (, ())
          <$> (buildModelContent useDescribe connection =<< readFile (m ^. Model.name))
      )
      (
        error "Static data only."
      )
