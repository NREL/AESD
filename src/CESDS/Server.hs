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


import CESDS.Types.Server (APIError(APIError))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad (liftM)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadIO, MonadReader, MonadTrans, ReaderT(runReaderT), ask, lift, liftIO)
import Data.Aeson (FromJSON, eitherDecode')
import Data.Default (def)
import Network.HTTP.Types (Status, badRequest400, internalServerError500)
import Network.Wai (Response)
import Network.Wai.Handler.Warp (Port, {- setOnException, -} setOnExceptionResponse, setPort)
import Web.Scotty.Trans (ActionT, Parsable, ScottyT, Options(..), body, defaultHandler, get, json, notFound, param, params, post, raise, scottyOptsT, status)

import qualified CESDS.Types as CESDS (Generation, Tags(..))
import qualified CESDS.Types.Bookmark as CESDS (Bookmark, BookmarkIdentifier)
import qualified CESDS.Types.Command as CESDS (Command, Result)
import qualified CESDS.Types.Filter as CESDS (Filter, FilterIdentifier)
import qualified CESDS.Types.Model as CESDS (Model, ModelIdentifier)
import qualified CESDS.Types.Record as CESDS (Record, RecordIdentifier)
import qualified CESDS.Types.Server as CESDS (Server)
import qualified CESDS.Types.Work as CESDS (Submission, SubmissionResult, WorkIdentifier, WorkStatus)

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as LT (Text, pack, unpack)
import qualified Network.Wai.Util as Wai (json)


data Service s =
  Service
  {
    getServer        :: ServerM s CESDS.Server
  , postServer       :: CESDS.Command -> ServerM s CESDS.Result
  , getModel         :: CESDS.ModelIdentifier -> ServerM s CESDS.Model
  , postModel        :: CESDS.Command -> CESDS.ModelIdentifier -> ServerM s CESDS.Result
  , getWorks         :: WorkFilter -> CESDS.ModelIdentifier -> ServerM s [CESDS.WorkStatus]
  , postWork         :: CESDS.Submission -> CESDS.ModelIdentifier -> ServerM s CESDS.SubmissionResult
  , getRecords       :: RecordFilter -> CESDS.ModelIdentifier -> ServerM s [CESDS.Record]
  , getBookmarkMetas :: CESDS.Tags -> CESDS.ModelIdentifier -> ServerM s [CESDS.Bookmark]
  , getBookmarks     :: Maybe CESDS.BookmarkIdentifier -> CESDS.ModelIdentifier -> ServerM s [CESDS.Bookmark]
  , postBookmark     :: CESDS.Bookmark -> CESDS.ModelIdentifier -> ServerM s CESDS.Bookmark
  , getFilterMetas   :: CESDS.Tags -> CESDS.ModelIdentifier -> ServerM s [CESDS.Filter]
  , getFilters       :: Maybe CESDS.FilterIdentifier -> CESDS.ModelIdentifier -> ServerM s [CESDS.Filter]
  , postFilter       :: CESDS.Filter -> CESDS.ModelIdentifier -> ServerM s CESDS.Filter
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
    rfFrom   :: Maybe CESDS.Generation
  , rfTo     :: Maybe CESDS.Generation
  , rfKey    :: Maybe String
  , rfRecord :: Maybe CESDS.RecordIdentifier
  }
    deriving (Eq, Read, Show)


runService :: Port -> Service s -> s -> IO ()
runService port Service{..} initial =
  runApplication port initial
    $ do
      defaultHandler $ \e -> apiError (badRequest400, LT.unpack e)
      get "/server"
        $ json =<< serverM getServer
      post "/server" . withBody
        $ (json =<<) . serverM . postServer
      get "/server/:model"
        $ json =<< serverM . getModel =<< param "model"
      post "/server/:model" . withBody
        $ (json =<<) . (param "model" >>=) . (serverM .) . postModel
      get "/server/:model/work"
        $ do
            model <- param "model"
            parameters <- map fst <$> params
            wfFrom   <- maybeParam parameters "from"
            wfTo     <- maybeParam parameters "to"
            wfStatus <- maybeParam parameters "status"
            wfWork   <- maybeParam parameters "work_id"
            json =<< serverM (getWorks WorkFilter{..} model)
      post "/server/:model/work" . withBody
        $ (json =<<) . (param "model" >>=) . (serverM .) . postWork
      get "/server/:model/records"
        $ do
            model <- param "model"
            parameters <- map fst <$> params
            rfFrom   <- maybeParam parameters "from"
            rfTo     <- maybeParam parameters "to"
            rfKey    <- maybeParam parameters "primary_key"
            rfRecord <- maybeParam parameters "result_id"
            json =<< serverM (getRecords RecordFilter{..} model)
      get "/server/:model/bookmark_metas"
        $ do
            model <- param "model"
            tags' <- tags
            json =<< serverM (getBookmarkMetas tags' model)
      get "/server/:model/bookmarks"
        $ do
            model <- param "model"
            parameters <- map fst <$> params
            bookmarkIdentifier <- maybeParam parameters "bookmark_id"
            json =<< serverM (getBookmarks bookmarkIdentifier model)
      post "/server/:model/bookmarks" . withBody
        $ (json =<<) . (param "model" >>=) . (serverM .) . postBookmark
      get "/server/:model/filter_metas"
        $ do
            model <- param "model"
            tags' <- tags
            json =<< serverM (getFilterMetas tags' model)
      get "/server/:model/filters"
        $ do
            model <- param "model"
            parameters <- map fst <$> params
            filterIdentifier <- maybeParam parameters "filter_id"
            json =<< serverM (getFilters filterIdentifier model)
      post "/server/:model/filters" . withBody
        $ (json =<<) . (param "model" >>=) . (serverM .) . postFilter
      notFound
        $ apiError (badRequest400, "invalid request")


tags :: Monad m => ActionT LT.Text m CESDS.Tags
tags =
  do
    parameters <- params
    CESDS.Tags
      <$> sequence
        [
          case eitherDecode' $ LBS.pack $ LT.unpack v of
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
  either (apiError . (badRequest400, )) f
    =<< eitherDecode'
    <$> body


apiError :: Monad m => (Status, String) -> ActionT LT.Text m ()
apiError (s, e) = json (APIError $ T.pack e) >> status s


newtype ServerM s a = ServerM {runServerM :: ExceptT String (ReaderT (TVar s) IO) a}
  deriving (Applicative, Functor, MonadError String, Monad, MonadIO, MonadReader (TVar s))


serverM :: MonadTrans t => ServerM s a -> t (ServerM s) a
serverM = lift


gets :: (s -> a) -> ServerM s a
gets f = liftM f $ ask >>= liftIO . readTVarIO


sets :: s -> ServerM s ()
sets = (ask >>=) . ((liftIO . atomically) .) . flip writeTVar


modifys :: (s -> s) -> ServerM s ()
modifys f = ask >>= liftIO . atomically . flip modifyTVar' f


modifysIO :: (s -> IO s) -> ServerM s ()
modifysIO f = -- FIXME: rewrite in pointfree style
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
exceptResponse = either (Wai.json badRequest400 [] . APIError . T.pack) return


options :: Port -> Options
options port =
  def
    {
      settings =
        setPort port
--        $ setOnException (const $ putStrLn . ("Uncaught exception: " ++) . show)
          $ setOnExceptionResponse (head . Wai.json internalServerError500 [] . APIError . T.pack . show)
          $ settings def
    , verbose  = 0
    }
