-----------------------------------------------------------------------------
-- |
-- Module      : Utility.Except
-- Description : Utility functions for exceptions in the `IO` monad
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Utility.Except
  ( liftEither
  ) where

import Control.Monad.Except


liftEither :: (MonadError e m) => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right a) = return a

