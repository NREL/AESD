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
import Database.HDBC (IConnection, describeResult, execute, fetchAllRows, prepare)
import Database.HDBC.ColTypes (SqlColDesc(..), SqlTypeId(..))
import Database.HDBC.SqlValue (SqlValue(SqlNull), fromSql)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.FilePath.Posix ((</>))


buildModelMeta :: IConnection c => c -> String -> IO ModelMeta
buildModelMeta connection query =
  do
    statement <- prepare connection $ "SELECT * FROM (" ++ query ++ ") AS buildModelMeta LIMIT 0"
    void $ execute statement []
    namesTypes <- fmap analyzeColumn <$> describeResult statement
    let
      vids = [1..]
    return
      $ def
      & varMeta
      .~ zipWith (\i (n, t, _) -> buildVarMeta i n t) vids namesTypes


buildModelContent :: IConnection c => c -> String -> IO [RecordContent]
buildModelContent connection query =
  do
    statement <- prepare connection query
    void $ execute statement []
    (_, _, casts) <- unzip3 . fmap analyzeColumn <$> describeResult statement
    values <- fetchAllRows statement
    let
      vids = [1..]
      rids = [1..]
    return
      $ zipWith (\rid vs -> (rid, zip vids vs)) rids
      $ zipWith ($) casts
      <$> values


analyzeColumn :: (String, SqlColDesc) -> (String, VarType, SqlValue -> DataValue)
analyzeColumn (column, SqlColDesc{..})
  | colType `elem` [SqlNumericT, SqlSmallIntT, SqlIntegerT, SqlTinyIntT, SqlBigIntT] = (column, IntegerVar, flip (integerValue .~) def . Just . fromSql')
  | colType `elem` [SqlDecimalT, SqlRealT, SqlFloatT, SqlDoubleT]                    = (column, RealVar   , flip (realValue    .~) def . Just . fromSql')
  | otherwise                                                                        = (column, StringVar , flip (stringValue  .~) def . Just . fromSql')
  where
    fromSql' SqlNull = def
    fromSql' x       = fromSql x


hdbcMain :: IConnection c => String -> Int -> FilePath -> Maybe FilePath -> c -> IO ()
hdbcMain host port directory persistence connection =
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
              <$> (buildModelMeta connection =<< readFile file)
          |
            file <- (directory' </>) <$> files
          , ".sql" `isSuffixOf` file
          ]
      )
      ( \_ m ->
        (, ())
          <$> (buildModelContent connection =<< readFile (m ^. Model.name))
      )
