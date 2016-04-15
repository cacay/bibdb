-----------------------------------------------------------------------------
-- |
-- Module      : Database.Doi
-- Description : Communication protocols for DOI
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.Doi (fetchString) where

import qualified Data.ByteString.Char8 as ByteString

import Network.Curl.Download (openURIWithOpts)
import Network.Curl.Opts

import Args (BibSize (..))
import Reference

import Utility.Except


fetchString :: BibSize -> SourceKey -> Exception String
fetchString size key = do
  res <- liftIO $ openURIWithOpts headers (getUrl size key)
  bs <- liftEither res
  return $ ByteString.unpack bs
  where
    headers = [ CurlFollowLocation True
              , CurlHttpHeaders ["Accept: application/x-bibtex"]
              ]


getUrl :: BibSize -> SourceKey -> String
getUrl size (SourceKey key) = "http://dx.doi.org/" ++ key

