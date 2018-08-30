{-|
Module      :  $Header$
Copyright   :  (c) 2018 Alliance for Sustainable Energy LLC
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

WebSockets with STM queues for sending and receiving messages.  There is one thread for sending messages, another thread for receiveing them, and separate threads to process the messages received from each request.  Each worker can also have its own thread.
-}


{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}


module Network.WebSockets.STM (
-- * Types
  Communicator
-- * Control
, start
, stop
, waitToStop
, complete
-- * Communication
, send
, launch
-- * Computation
, forkWork
) where


import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (finally, handle)
import Control.Monad (forever, when, unless)
import Data.ByteString.Char8 (pack)
import Data.Map.Strict as M (Map, delete, elems, empty, insert, lookup)
import Data.Set as S (Set, delete, elems, empty, insert)
import Network.WebSockets (Connection, ConnectionException, WebSocketsData, receiveData, sendBinaryData, sendClose)


-- | Whether to write debugging messages.
debug :: Bool
#ifdef AESD_VERBOSE
debug = True
#else
debug = False
#endif


-- | The state of communication via WebSockets.
data Communicator s r k =
  Communicator
  {
    connection :: Connection                        -- ^ The WebSockets connections.
  , sender     :: ThreadId                          -- ^ Thread for sending messages.
  , sends      :: TQueue s                          -- ^ Queue of messages to be sent.
  , receiver   :: ThreadId                          -- ^ Thread for receiving messages.
  , writeKey   :: k -> s -> s                       -- ^ Function for adding the key to a message.
  , processors :: TVar (Map k (ThreadId, TQueue r)) -- ^ Map of threads and receiving queues for each incoming message key.
  , workers    :: TVar (Set ThreadId)               -- ^ Worker threads.
  , stopped    :: MVar ()                           -- ^ Whether the threads are stopped.
  }


-- | Fork the threads needed for communication.
start :: (WebSocketsData s, WebSocketsData r, Ord k)
      => Connection              -- ^ The WebSockets connection.
      -> (k -> s -> s)           -- ^ Function for adding the key to a message.
      -> (r -> k)                -- ^ Function for getting the key of a message.
      -> IO (Communicator s r k) -- ^ Action to create the threads and start communication.
start connection writeKey readKey =
  do
    workers <- newTVarIO S.empty
    stopped <- newEmptyMVar
    processors <- newTVarIO M.empty
    -- Fork a thread for sending messages from a queue.
    sends <- newTQueueIO
    sender <-
      forkIO
        . handle (\e -> when debug $ putStrLn ("SENDER: " ++ show (e :: ConnectionException)))
        . forever
        $ sendBinaryData connection
        =<< atomically (readTQueue sends)
    -- Fork a thread for receiving messages and dispatching them to a specific queue.
    receiver <-
      forkIO
        . handle (\e -> when debug (putStrLn $ "RECEIVER: " ++ show (e :: ConnectionException)) >> putMVar stopped ())
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
stop communicator@Communicator{..} =
  do
    sendClose connection $ pack "Normal stop."
    waitToStop communicator


-- | Wait for the WebSockets connection to be closed.
waitToStop :: Communicator s r k -> IO ()
waitToStop Communicator{..} =
  do
    -- Tell WebSockets to close the connection, which will also result in the receiving thread exiting.
    readMVar stopped
    -- Kill the workers.
    mapM_ safelyKillThread -- FIXME: Consider replacing the killing of threads with the killing of thread groups.
      =<< S.elems
      <$> readTVarIO workers
    -- Kill the processors.
    mapM_ (safelyKillThread . fst)
      =<< M.elems
      <$> readTVarIO processors
    -- Kill the sender.
    safelyKillThread sender


-- | Safely kill a thread.
safelyKillThread :: ThreadId -> IO ()
safelyKillThread threadId =
  killThread threadId
    `finally` return ()


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
       -> k                  -- ^ The message key.
       -> (r -> IO Bool)     -- ^ Handler for messages, which returns whether the handling of this message key is complete.
       -> IO ()              -- ^ Action for processing incoming messages.
launch Communicator{..} key responder =
  do
    -- Create the incoming message queue.
    receipts <- newTQueueIO
    -- Repeatedly respond to messages.
    let
      loop =
        atomically (readTQueue receipts)
          >>= responder
          >>= (`unless` loop)
    thread <-
      forkIO
        $ loop
        >> mute processors key
    -- Record the thread and queue information.
    atomically
      . modifyTVar' processors
      $ M.insert key (thread, receipts)


-- | Fork a worker thread.
forkWork :: Communicator s r k -> IO () -> IO ()
forkWork Communicator{..} worker =
  do
    threadMVar <- newEmptyMVar
    thread <-
      forkIO
        . finally worker
        $ do
          thread' <- takeMVar threadMVar
          atomically
            . modifyTVar' workers
            $ S.delete thread'
    atomically
      $ modifyTVar' workers
      (S.insert thread)
    putMVar threadMVar thread
    

-- | Stop processing messages with a particular key.
complete :: Ord k
         => Communicator s r k -- ^ The state of communication.
         -> k                  -- ^ The message key.
         -> IO ()              -- ^ Action for stopping the processing of messages with the key.
complete Communicator{..} key =
  mute processors key
    >> lookupThread processors key
    >>= maybe (return ()) safelyKillThread


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
