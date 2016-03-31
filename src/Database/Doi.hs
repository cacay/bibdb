-----------------------------------------------------------------------------
-- |
-- Module      : Database.Doi
-- Description : Communication protocols for DOI
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.Doi (fetch, fetchString) where

import qualified Data.ByteString.Char8 as ByteString

import Network.Curl.Download (openURIWithOpts)
import Network.Curl.Opts

import Args (BibSize (..))
import Reference

import Utility.Except


fetch :: BibSize -> SourceKey -> Exception (RefIdent, [BibTeX])
fetch size key = do
  bibstr <- fetchString size key
  case parseBibTeX bibstr of
    Left err -> throwError $ "Error while parsing the server result:\n" ++ err
    Right bibtex@(h : _) -> return (bibIdent h, bibtex)
    Right _ -> throwError $ "Invalid response from server"


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

