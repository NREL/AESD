{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}


module CESDS.Server (
  Service(..)
, WorkFilter(..)
, RecordFilter(..)
, runService
, Port
, ServerM(..)
, serverM
, gets
, sets
, modifys
, modifysIO
) where


import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadIO, MonadReader, MonadTrans, ReaderT(runReaderT), ask, lift, liftIO)
import Data.Aeson (eitherDecode')
import Data.Aeson.Types (FromJSON, Value(String), (.=), object)
import Data.Default (def)
import Data.List ((\\))
import Data.List.Util (hasSubset)
import Network.HTTP.Types (Status, badRequest400, internalServerError500)
import Network.Wai (Response)
import Network.Wai.Handler.Warp (Port, {- setOnException, -} setOnExceptionResponse, setPort)
import Web.Scotty.Trans (ActionT, Parsable, ScottyT, Options(..), body, defaultHandler, delete, get, json, notFound, param, params, post, raise, scottyOptsT, status, text)

import qualified CESDS.Types as CESDS (Generation, Tags(..))
import qualified CESDS.Types.Bookmark as CESDS (Bookmark, BookmarkIdentifier, BookmarkList)
import qualified CESDS.Types.Command as CESDS (Command, Result)
import qualified CESDS.Types.Filter as CESDS (Filter, FilterIdentifier, FilterList)
import qualified CESDS.Types.Model as CESDS (Model, ModelIdentifier)
import qualified CESDS.Types.Record as CESDS (Record, RecordIdentifier, RecordList)
import qualified CESDS.Types.Server as CESDS (Server)
import qualified CESDS.Types.Variable as CESDS (VariableIdentifier)
import qualified CESDS.Types.Work as CESDS (Submission, SubmissionResult, Work, WorkIdentifier, WorkList)

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as LT (Text, concat, head, intercalate, pack, unpack)
import qualified Network.Wai.Util as Wai (text)


data Service s =
  Service
  {
    getServer       :: ServerM s CESDS.Server
  , postServer      :: CESDS.Command -> ServerM s CESDS.Result
  , getModel        :: CESDS.ModelIdentifier -> ServerM s CESDS.Model
  , postModel       :: CESDS.Command -> CESDS.ModelIdentifier -> ServerM s CESDS.Result
  , getRecord       :: RecordFilter -> CESDS.ModelIdentifier -> ServerM s CESDS.Record
  , getRecords      :: RecordFilter -> CESDS.ModelIdentifier -> ServerM s CESDS.RecordList
  , postRecord      :: CESDS.Record -> CESDS.ModelIdentifier -> ServerM s ()
  , getWork         :: WorkFilter -> CESDS.ModelIdentifier -> ServerM s CESDS.Work
  , getWorks        :: WorkFilter -> CESDS.ModelIdentifier -> ServerM s CESDS.WorkList
  , postWork        :: CESDS.Submission -> CESDS.ModelIdentifier -> ServerM s CESDS.SubmissionResult
  , deleteWork      :: WorkFilter -> CESDS.ModelIdentifier -> ServerM s ()
  , getBookmarkList :: CESDS.Tags -> CESDS.ModelIdentifier -> ServerM s CESDS.BookmarkList
  , getBookmark     :: CESDS.BookmarkIdentifier -> CESDS.ModelIdentifier -> ServerM s CESDS.Bookmark
  , postBookmark    :: CESDS.Bookmark -> CESDS.ModelIdentifier -> ServerM s CESDS.Bookmark
  , deleteBookmark  :: CESDS.BookmarkIdentifier -> CESDS.ModelIdentifier -> ServerM s ()
  , getFilterList   :: CESDS.Tags -> CESDS.ModelIdentifier -> ServerM s CESDS.FilterList
  , getFilter       :: CESDS.FilterIdentifier -> CESDS.ModelIdentifier -> ServerM s CESDS.Filter
  , postFilter      :: CESDS.Filter -> CESDS.ModelIdentifier -> ServerM s CESDS.Filter
  , deleteFilter    :: CESDS.FilterIdentifier -> CESDS.ModelIdentifier -> ServerM s ()
  }


data WorkFilter =
  WorkFilter
  {
    wfFrom   :: Maybe CESDS.Generation
  , wfTo     :: Maybe CESDS.Generation
  , wfStatus :: Maybe String
  , wfWork   :: Maybe CESDS.WorkIdentifier
  }
    deriving (Eq, Read, Show)


data RecordFilter = 
  RecordFilter
  {
    rfFrom      :: Maybe CESDS.Generation
  , rfTo        :: Maybe CESDS.Generation
  , rfKey       :: Maybe String
  , rfRecord    :: Maybe CESDS.RecordIdentifier
  , rfVariables :: Maybe [CESDS.VariableIdentifier]
  }
    deriving (Eq, Read, Show)


runService :: Port -> Service s -> s -> IO ()
runService port Service{..} initial =
  runApplication port initial
    $ do
      defaultHandler $ \e -> apiError (badRequest400, LT.unpack e)
      get "/"
        $ json =<< serverM getServer
      post "/command" . withBody
        $ (json =<<) . serverM . postServer
      get "/models/:model"
        $ json =<< serverM . getModel . fst =<< params0 []
      post "/models/:model/command" . withBody
        $ \b -> do
            (modelIdentifier, _) <- params0 []
            json =<< serverM (postModel b modelIdentifier)
      get "/models/:model/records/:result_id"
        $ do
            let (rfFrom, rfTo, rfKey) = (Nothing, Nothing, Nothing)
            (modelIdentifier, rfRecord, rfVariables, _) <- params2 ["result_id", "variables"]
            json =<< serverM (getRecord RecordFilter{..} modelIdentifier)
      get "/models/:model/records"
        $ do
            let rfRecord = Nothing
            (modelIdentifier, rfFrom, rfTo, rfKey, rfVariables, _) <- params4 ["from", "to", "primary_key", "variables"]
            json =<< serverM (getRecords RecordFilter{..} modelIdentifier)
      post "/models/:model/records" . withBody
        $ \b -> do
            (modelIdentifier, _) <- params0 []
            serverM $ postRecord b modelIdentifier
            json $ object ["result" .= String "ok"]
      get "/models/:model/work/:work_id"
        $ do
            let (wfFrom, wfTo, wfStatus) = (Nothing, Nothing, Nothing)
            (modelIdentifier, wfWork, _) <- params1 ["work_id"]
            json =<< serverM (getWork WorkFilter{..} modelIdentifier)
      get "/models/:model/work"
        $ do
            let wfWork = Nothing
            (modelIdentifier, wfFrom, wfTo, wfStatus, _) <- params3 ["from", "to", "status"]
            json =<< serverM (getWorks WorkFilter{..} modelIdentifier)
      post "/models/:model/work" . withBody
        $ \b -> do
            (modelIdentifier, _) <- params0 []
            json =<< serverM (postWork b modelIdentifier)
      delete "/models/:model/work/:work_id"
        $ do
            let (wfFrom, wfTo, wfStatus) = (Nothing, Nothing, Nothing)
            (modelIdentifier, wfWork, _) <- params1 ["work_id"]
            json =<< serverM (deleteWork WorkFilter{..} modelIdentifier)
      get "/models/:model/bookmarks"
        $ do
            (modelIdentifier, tags) <- paramsTags
            json =<< serverM (getBookmarkList tags modelIdentifier)
      get "/models/:model/bookmarks/:bookmark_id"
        $ do
            (modelIdentifier, Just bookmarkIdentifier, _) <- params1 ["bookmark_id"]
            json =<< serverM (getBookmark bookmarkIdentifier modelIdentifier)
      post "/models/:model/bookmarks" . withBody
        $ \b -> do
            (modelIdentifier, _) <- params0 []
            json =<< serverM (postBookmark b modelIdentifier)
      delete "/models/:model/bookmarks/:bookmark_id"
        $ do
            (modelIdentifier, Just bookmarkIdentifier, _) <- params1 ["bookmark_id"]
            json =<< serverM (deleteBookmark bookmarkIdentifier modelIdentifier)
      get "/models/:model/filters"
        $ do
            (modelIdentifier, tags) <- paramsTags
            json =<< serverM (getFilterList tags modelIdentifier)
      get "/models/:model/filters/:filter_id"
        $ do
            (modelIdentifier, Just filterIdentifier, _) <- params1 ["filter_id"]
            json =<< serverM (getFilter filterIdentifier modelIdentifier)
      post "/models/:model/filters" . withBody
        $ \b -> do
            (modelIdentifier, _) <- params0 []
            json =<< serverM (postFilter b modelIdentifier)
      delete "/models/:model/filters/:filter_id"
        $ do
            (modelIdentifier, Just filterIdentifier, _) <- params1 ["filter_id"]
            json =<< serverM (deleteFilter filterIdentifier modelIdentifier)
      notFound
        $ apiError (badRequest400, "invalid request")


params0 :: MonadIO m => [LT.Text] -> ActionT LT.Text m (CESDS.ModelIdentifier, [LT.Text])
params0 ps =
  do
    parameters <- filter ((/= '{') . LT.head) . map fst <$> params
    unless (("model" : ps) `hasSubset` parameters)
      . raise
      $ LT.concat ["illegal parameters in URL: ", LT.intercalate ", " $ parameters \\ ("model" : ps)]
    modelIdentifier <- param "model"
    return (modelIdentifier, parameters)


params1 :: (MonadIO m, Parsable a) => [LT.Text] -> ActionT LT.Text m (CESDS.ModelIdentifier, Maybe a, [LT.Text])
params1 ps =
  do
    (modelIdentifier, parameters) <- params0 ps
    v1 <- maybeParam parameters $ head ps
    return (modelIdentifier, v1, parameters)


params2 :: (MonadIO m, Parsable a, Parsable b) => [LT.Text] -> ActionT LT.Text m (CESDS.ModelIdentifier, Maybe a, Maybe b, [LT.Text])
params2 ps =
  do
    (modelIdentifier, v2, parameters) <- params1 $ tail ps ++ [head ps]
    v1 <- maybeParam parameters $ head ps
    return (modelIdentifier, v1, v2, parameters)


params3 :: (MonadIO m, Parsable a, Parsable b, Parsable c) => [LT.Text] -> ActionT LT.Text m (CESDS.ModelIdentifier, Maybe a, Maybe b, Maybe c, [LT.Text])
params3 ps =
  do
    (modelIdentifier, v2, v3, parameters) <- params2 $ tail ps ++ [head ps]
    v1 <- maybeParam parameters $ head ps
    return (modelIdentifier, v1, v2, v3, parameters)


params4 :: (MonadIO m, Parsable a, Parsable b, Parsable c, Parsable d) => [LT.Text] -> ActionT LT.Text m (CESDS.ModelIdentifier, Maybe a, Maybe b, Maybe c, Maybe d, [LT.Text])
params4 ps =
  do
    (modelIdentifier, v2, v3, v4, parameters) <- params3 $ tail ps ++ [head ps]
    v1 <- maybeParam parameters $ head ps
    return (modelIdentifier, v1, v2, v3, v4, parameters)


paramsTags :: Monad m => ActionT LT.Text m (CESDS.ModelIdentifier, CESDS.Tags)
paramsTags =
  do
    modelIdentifier <- param "model"
    parameters <- params
    (modelIdentifier, )
      . CESDS.Tags
      <$> sequence
        [
          case eitherDecode' . LBS.pack $ LT.unpack v of
            Left msg -> raise $ LT.pack msg
            Right v' -> return (T.pack $ LT.unpack k, v')
        |
          (k, v) <- parameters
        , k /= "model"
        ]


maybeParam :: (Monad m, Parsable a) => [LT.Text] -> LT.Text -> ActionT LT.Text m (Maybe a)
maybeParam ps p =
  if p `elem` ps
    then Just <$> param p
    else return Nothing


withBody :: (FromJSON a, MonadIO m) => (a -> ActionT LT.Text m ()) -> ActionT LT.Text m ()
withBody f =
  either (apiError . (badRequest400, ) . ("illegal JSON: " ++)) f
    =<< eitherDecode'
    <$> body


apiError :: Monad m => (Status, String) -> ActionT LT.Text m ()
apiError (s, e) = text (LT.pack e) >> status s


newtype ServerM s a = ServerM {runServerM :: ExceptT String (ReaderT (TVar s) IO) a}
  deriving (Applicative, Functor, MonadError String, Monad, MonadIO, MonadReader (TVar s))


serverM :: MonadTrans t => ServerM s a -> t (ServerM s) a
serverM = lift


gets :: (s -> a) -> ServerM s a
gets f = f <$> (ask >>= liftIO . readTVarIO)


sets :: s -> ServerM s ()
sets = (ask >>=) . ((liftIO . atomically) .) . flip writeTVar


modifys :: (s -> s) -> ServerM s ()
modifys f = ask >>= liftIO . atomically . flip modifyTVar' f


modifysIO :: (s -> IO s) -> ServerM s ()
modifysIO f = -- TODO: Rewrite in pointfree style.
  do
    sTVar <- ask
    s <- liftIO $ readTVarIO sTVar
    s' <- liftIO $ f s
    liftIO . atomically $ writeTVar sTVar s'

 
type Application s = ScottyT LT.Text (ServerM s) ()

      
runApplication :: Port -> s -> Application s -> IO ()
runApplication port initial application =
  do
    sync <- newTVarIO initial
    let
      runActionToIO m = runReaderT (runExceptT (runServerM m)) sync
    scottyOptsT (options port) ((exceptResponse =<<) . runActionToIO) application


exceptResponse :: Either String Response -> IO Response
exceptResponse = either (Wai.text badRequest400 [] . T.pack) return


options :: Port -> Options
options port =
  def
    {
      settings =
        setPort port
--        $ setOnException (const $ putStrLn . ("Uncaught exception: " ++) . show)
          $ setOnExceptionResponse (head . Wai.text internalServerError500 [] . T.pack . show)
          $ settings def
    , verbose  = 0
    }
