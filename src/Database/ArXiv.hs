-----------------------------------------------------------------------------
-- |
-- Module      : Database.ArXiv
-- Description : Communication protocols for arXiv
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.ArXiv (fetchString) where

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
    headers = [ CurlFollowLocation True ]


getUrl :: BibSize -> SourceKey -> String
getUrl size (SourceKey key) =
  "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&arXiv:" ++ key

