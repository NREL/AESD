module Control.Monad.Except.Util (
  assert
) where


import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Data.String (IsString)


assert :: (IsString e, MonadError e m) => e -> Bool -> m ()
assert = flip unless . throwError
