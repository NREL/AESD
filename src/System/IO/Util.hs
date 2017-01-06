{-# LANGUAGE FlexibleContexts #-}


module System.IO.Util (
  guardIO
) where


import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import System.IO.Error (tryIOError)


-- See <http://chromaticleaves.com/posts/guard-io-with-errort.html>.
guardIO :: (MonadIO m, MonadError String m) => IO a -> m a
guardIO =
  (either (throwError . show) return =<<)
    . liftIO
    . tryIOError
