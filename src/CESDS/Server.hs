{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


module CESDS.Server (
  Service(..)
, runService
, Port
) where


import CESDS.Types.Server (Server)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (liftM)
import Control.Monad.Reader (MonadIO, MonadReader, MonadTrans, ReaderT(runReaderT), ask, lift, liftIO)
import Data.Default (def)
import Data.Text.Lazy (Text)
import Network.Wai.Handler.Warp (Port, setPort)
import Web.Scotty.Trans (ScottyT, Options(..), get, json, scottyOptsT)


data Service s =
  Service
  {
    getServer :: s -> Server
  }


runService :: Port -> Service s -> s -> IO ()
runService port Service{..} initial =
  runApplication port initial
    $ do
      get "/server"
        $ json =<< serverM (gets getServer)


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
