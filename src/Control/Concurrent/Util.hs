module Control.Concurrent.Util (
  makeCounter
) where


import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)


makeCounter :: (a -> a) -> a -> IO (IO a)
makeCounter f i =
  do
    counter <- newTVarIO i
    let
      next =
        atomically
          $ do
            j <- f <$> readTVar counter
            writeTVar counter j
            return j
    return next
