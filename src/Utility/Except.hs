-----------------------------------------------------------------------------
-- |
-- Module      : Utility.Except
-- Description : Utility functions for exceptions in the `IO` monad
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Utility.Except
  ( module Control.Monad.Except
  , MonadIO (..)
  , Exception
  , liftEither
  ) where
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))


type Exception = ExceptT String IO


liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither (Left e) = throwError e
liftEither (Right a) = return a

