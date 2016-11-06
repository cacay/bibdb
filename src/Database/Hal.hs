-----------------------------------------------------------------------------
-- |
-- Module      : Database.Hal
-- Description : Communication protocols for HAL and Inria
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.Hal (fetchString) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))

import qualified Data.ByteString.Char8 as ByteString

import Network.Curl.Download (openURIWithOpts)
import Network.Curl.Opts

import Args (BibSize (..))
import Reference

import Utility.Except


fetchString :: (MonadError String m, MonadIO m) => BibSize -> SourceKey -> m String
fetchString size key = do
  res <- liftIO $ openURIWithOpts headers (getUrl size key)
  bs <- liftEither res
  return $ ByteString.unpack bs
  where
    headers = [ CurlFollowLocation True ]


getUrl :: BibSize -> SourceKey -> String
getUrl size (SourceKey key) =
  "http://hal.inria.fr/" ++ key ++ "/bibtex"

