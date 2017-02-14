{-|
Module      :  $Header$
Copyright   :  (c) 2017 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

WebSockets with STM queues for sending and receiving messages.  There is one thread for sending messages, another thread for receiveing them, and separate threads to process the messages received from each request.
-}


{-# LANGUAGE RecordWildCards #-}


module Network.WebSockets.STM (
-- * Types
  Communicator
-- * Control
, start
, stop
, complete
-- * Communication
, send
, launch
) where


import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (forever, unless)
import Data.Map.Strict as M (Map, delete, elems, empty, insert, lookup)
import Network.WebSockets (Connection, WebSocketsData, receiveData, sendBinaryData)


-- | The state of communication via WebSockets.
data Communicator s r k =
  Communicator
  {
    sender     :: ThreadId                          -- ^ Thread for sending messages.
  , sends      :: TQueue s                          -- ^ Queue of messages to be sent.
  , receiver   :: ThreadId                          -- ^ Thread for receiving messages.
  , writeKey   :: k -> s -> s                       -- ^ Function for adding the key to a message.
  , processors :: TVar (Map k (ThreadId, TQueue r)) -- ^ Map of threads and receiving queues for each incoming message key.
  }


-- | Fork the threads needed for communication.
start :: (WebSocketsData s, WebSocketsData r, Ord k)
      => Connection              -- ^ The WebSockets connection.
      -> (k -> s -> s)           -- ^ Function for adding the key to a message.
      -> (r -> k)                -- ^ Function for getting the key of a message.
      -> IO (Communicator s r k) -- ^ Action to create the threads and start communication.
start connection writeKey readKey =
  do
    processors <- newTVarIO M.empty
    sends <- newTQueueIO
    sender <-
      forkIO
        . forever
        $ sendBinaryData connection
        =<< atomically (readTQueue sends)
    receiver <-
      forkIO
        . forever
        $ do
          x <- receiveData connection
          maybeProcessor <- lookupReceipt processors $ readKey x
          case maybeProcessor of
            Nothing       -> return ()
            Just receipts -> atomically $ writeTQueue receipts x
          readTVarIO processors
    return Communicator{..}


-- | Kill the communication threads.
stop :: Communicator s r k -> IO ()
stop Communicator{..} =
  do
    mapM_ (killThread . fst)
      =<< elems
      <$> readTVarIO processors
    killThread receiver
    killThread sender


-- | Send a message.
send :: Communicator s r k -- ^ The state of communication.
     -> k                  -- ^ The message key.
     -> s                  -- ^ The message to be sent.
     -> IO ()              -- ^ Action for enqueuing the message for sending.
send Communicator{..} key =
  atomically
    . writeTQueue sends
    . writeKey key


-- | Process messages for a given key.
launch :: Ord k
       => Communicator s r k -- ^ The state of communication.
      -> k                   -- ^ The message key.
      -> (r -> IO Bool)      -- ^ Handler for messages, which returns whether the handling of this message key is complete.
      -> IO ()               -- ^ Action for processing incoming messages.
launch Communicator{..} key responder =
  do
    receipts <- newTQueueIO
    let
      loop =
        atomically (readTQueue receipts)
          >>= responder
          >>= (`unless` loop)
    thread <-
      forkIO
        $ loop
        >> mute processors key
    atomically
      . modifyTVar' processors
      $ M.insert key (thread, receipts)


-- | Stop processing messages with a particular key.
complete :: Ord k
         => Communicator s r k -- ^ The state of communication.
         -> k                  -- ^ The message key.
         -> IO ()              -- ^ Action for stopping the processing of messages with the key.
complete Communicator{..} key =
  mute processors key
    >> lookupThread processors key
    >>= maybe (return ()) killThread


-- | Lookup the thread for processing a particular key.
lookupThread :: Ord k => TVar (Map k (ThreadId, TQueue r)) -> k -> IO (Maybe ThreadId)
lookupThread processors key =
  fmap fst
    . M.lookup key
    <$> readTVarIO processors


-- | Lookup the queue of received messages for a particular key.
lookupReceipt :: Ord k => TVar (Map k (ThreadId, TQueue r)) -> k -> IO (Maybe (TQueue r))
lookupReceipt processors key =
  fmap snd
    . M.lookup key
    <$> readTVarIO processors


-- | Stop receiving messages for a particular key.
mute :: Ord k => TVar (Map k (ThreadId, TQueue r)) -> k -> IO ()
mute processors =
  atomically
    . modifyTVar' processors
    . M.delete
