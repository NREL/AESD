{-|
Module      :  $Header$
Copyright   :  (c) 2017-18 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

Support serving of records from databases.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}


module AESD.Records.Server.HDBC (
-- * Entry point
  hdbcMain
-- * Metadata
, buildVarMetas
, analyzeColumn
-- * Data
, buildModelContent
) where


import AESD.Records.Server (serverMain)
import AESD.Records.Server.File (buildVarMeta)
import AESD.Records.Server.Manager (makeInMemoryManager)
import AESD.Types.Model as Model (makeModelMeta, name)
import AESD.Types.Record (RecordContent)
import AESD.Types.Value (DataValue, VarType(..), integerValue, realValue, stringValue)
import AESD.Types.Variable (VarMeta)
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


-- | Run the server.
hdbcMain :: IConnection c
         => Bool           -- ^ Whether to use the 'Database.HDBC.describeResult'.
         -> String         -- ^ The WebSocket host address.
         -> Int            -- ^ The WebSocket port number.
         -> FilePath       -- ^ The directory where SQL queries reside.
         -> Maybe FilePath -- ^ The location of the peristence journal file.
         -> Maybe Int      -- ^ The number of records per chunk.
         -> c              -- ^ The database connection.
         -> IO ()          -- ^ Action to run the server.
hdbcMain useDescribe host port directory persistence chunkSize connection =
  do
    directory' <- makeAbsolute directory
    files <- getDirectoryContents directory'
    serverMain host port chunkSize
      =<< makeInMemoryManager persistence ()
      (
        const
          . fmap (, ())
          $ sequence
          [
            flip
              (
                makeModelMeta
                (toString (generateNamed namespaceURL $ toEnum . fromEnum <$> "file://" ++ file))
                file
                ("file://" ++ file)
              )
              []
              <$> (buildVarMetas useDescribe connection =<< readFile file)
          |
            file <- (directory' </>) <$> files
          , ".sql" `isSuffixOf` file
          ]
      )
      ( \_ m ->
        (, ())
          <$> (buildModelContent useDescribe connection =<< readFile (Model.name m))
      )
      (
        error "Static data only."
      )


-- | Construct metadata for variables in a query.
buildVarMetas :: IConnection c
              => Bool         -- ^ Whether to use the 'Database.HDBC.describeResult'.
              -> c            -- ^ The database connection.
              -> String       -- ^ The SQL query.
              -> IO [VarMeta] -- ^ The action to return the metadata for variables.
buildVarMetas useDescribe connection query =
  do
    statement <- prepare connection $ "SELECT * FROM (" ++ query ++ ") AS buildModelMeta LIMIT " ++ (if useDescribe then "0" else "1")
    void $ execute statement []
    namesTypes <- analyzeColumns useDescribe connection statement
    let
      vids = [1..]
    return
      $ zipWith (\i (n, t, _) -> buildVarMeta i n t) vids namesTypes


-- | Construct the record data.
buildModelContent :: IConnection c
                  => Bool               -- ^ Whether to use the 'Database.HDBC.describeResult'.
                  -> c                  -- ^ The database connection.
                  -> String             -- ^ The SQL query.
                  -> IO [RecordContent] -- ^ Action to return the record data.
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


-- | Analyze columns.
analyzeColumns :: IConnection c
               => Bool                                          -- ^ Whether to use the 'Database.HDBC.describeResult'.
               -> c                                             -- ^ The database connection.
               -> Statement                                     -- ^ The prepared statement.
               -> IO [(String, VarType, SqlValue -> DataValue)] -- ^ Action to return the column names, types, and cast functions.
analyzeColumns True _ statement = fmap analyzeColumn <$> describeResult statement
analyzeColumns False connection statement =
  do
    statement' <- prepare connection $ originalQuery statement
    void $ execute statement' []
    maybe [] (fmap analyzeColumn') <$> fetchRowAL statement'


-- | Analyze a column.
analyzeColumn :: (String, SqlColDesc)                     -- ^ The name and column description.
              -> (String, VarType, SqlValue -> DataValue) -- ^ The column name, type, and cast function.
analyzeColumn (column, SqlColDesc{..}) = analyzeType column colType
  

-- | Analyze a column.
analyzeColumn' :: (String, SqlValue)                       -- ^ The name and a sample value.
               -> (String, VarType, SqlValue -> DataValue) -- ^ The column name, type, and cast function.
analyzeColumn' (column, SqlWord32   _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlWord64   _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlInt32    _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlInt64    _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlInteger  _) = analyzeType column SqlIntegerT
analyzeColumn' (column, SqlDouble   _) = analyzeType column SqlDoubleT 
analyzeColumn' (column, SqlRational _) = analyzeType column SqlDoubleT 
analyzeColumn' (column, _            ) = analyzeType column SqlVarCharT


-- | Analyze an SQL type.
analyzeType :: String                                   -- ^ The column name.
            -> SqlTypeId                                -- ^ The SQL type.
            -> (String, VarType, SqlValue -> DataValue) -- ^ The column name, type, and cast function.
analyzeType column typ
  | typ `elem` [SqlNumericT, SqlSmallIntT, SqlIntegerT, SqlTinyIntT, SqlBigIntT] = (column, IntegerVar, integerValue . fromSql')
  | typ `elem` [SqlDecimalT, SqlRealT, SqlFloatT, SqlDoubleT]                    = (column, RealVar   , realValue    . fromSql')
  | otherwise                                                                    = (column, StringVar , stringValue  . fromSql')
  where
    fromSql' SqlNull = def
    fromSql' x       = fromSql x
