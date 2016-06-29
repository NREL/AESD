{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}


module CESDS.Server (
  Service(..)
, runService
, Port
, ServerM(..)
, serverM
, gets
, modifys
) where


import CESDS.Types.Server (APIError(APIError))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (liftM)
import Control.Monad.Reader (MonadIO, MonadReader, MonadTrans, ReaderT(runReaderT), ask, lift, liftIO)
import Data.Aeson (FromJSON, eitherDecode')
import Data.Default (def)
import Data.Text (pack)
import Data.Text.Lazy (Text)
import Network.HTTP.Types (Status, badRequest400, notFound404)
import Network.Wai.Handler.Warp (Port, setPort)
import Web.Scotty.Trans (ActionT, ScottyT, Options(..), body, capture, get, json, param, post, scottyOptsT, status)

import qualified CESDS.Types.Command as CESDS (Command, Result)
import qualified CESDS.Types.Model as CESDS (Model, ModelIdentifier)
import qualified CESDS.Types.Server as CESDS (Server)


data Service s =
  Service
  {
    getServer  :: ServerM s CESDS.Server
  , postServer :: CESDS.Command -> ServerM s CESDS.Result
  , getModel   :: CESDS.ModelIdentifier -> ServerM s (Maybe CESDS.Model)
  , postModel  :: CESDS.Command -> CESDS.ModelIdentifier -> ServerM s CESDS.Result
  }


runService :: Port -> Service s -> s -> IO ()
runService port Service{..} initial =
  runApplication port initial
    $ do
      get "/server"
        $ json =<< serverM getServer
      post "/server" . withBody
        $ (json =<<) . serverM . postServer
      get (capture "/server/:model")
        $ maybeApiError (notFound404, "model not found") json =<< serverM . getModel =<< param "model"
      post (capture "/server/:model") . withBody
        $ (json =<<) . (param "model" >>=) . (serverM .) . postModel


withBody :: (FromJSON a, MonadIO m) => (a -> ActionT Text m ()) -> ActionT Text m ()
withBody f =
  either (apiError . (badRequest400, )) f
    =<< eitherDecode'
    <$> body


maybeApiError :: Monad m => (Status, String) -> (a -> ActionT Text m ()) -> Maybe a -> ActionT Text m ()
maybeApiError = maybe . apiError


apiError :: Monad m => (Status, String) -> ActionT Text m ()
apiError (s, e) = json (APIError $ pack e) >> status s


newtype ServerM s a = ServerM {runServerM :: ReaderT (TVar s) IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar s))


serverM :: MonadTrans t => ServerM s a -> t (ServerM s) a
serverM = lift


gets :: (s -> a) -> ServerM s a
gets f = liftM f $ ask >>= liftIO . readTVarIO


modifys :: (s -> s) -> ServerM s ()
modifys f = ask >>= liftIO . atomically . flip modifyTVar' f


type Application s = ScottyT Text (ServerM s) ()

      
runApplication :: Port -> s -> Application s -> IO ()
runApplication port initial application =
  do
    sync <- newTVarIO initial
    let
      runActionToIO m = runReaderT (runServerM m) sync
    scottyOptsT (options port) runActionToIO application


options :: Port -> Options
options port =
  def
    {
      settings = setPort port $ settings def
    , verbose  = 0
    }
