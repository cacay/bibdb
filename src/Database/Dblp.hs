-----------------------------------------------------------------------------
-- |
-- Module      : Database.Dblp
-- Description : Communication protocols for dblp
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.Dblp (fetch, fetchString) where

import Network.Curl.Download (openURIString)

import Args (BibSize (..))
import Reference

import Utility.Except


fetch :: BibSize -> SourceKey -> Exception (RefIdent, [BibTeX])
fetch size key = do
  bibstr <- fetchString size key
  let source = Source Dblp key
  case parseBibTeX bibstr of
    Left err -> throwError $ "Error while parsing the results of "
      ++ show source ++ ":\n" ++ err
    Right bibtex@(h : _) -> return (bibIdent h, bibtex)
    Right _ -> throwError $ "Invalid response from server for "
      ++ show source


fetchString :: BibSize -> SourceKey -> Exception String
fetchString size key = do
  res <- liftIO $ openURIString (getUrl size key)
  liftEither res


getUrl :: BibSize -> SourceKey -> String
getUrl size (SourceKey key) =
  "http://dblp.uni-trier.de/rec/" ++ parseSize size ++ "/" ++ key ++ ".bib"
  where parseSize :: BibSize -> String
        parseSize Condensed = "bib0"
        parseSize Standard  = "bib1"
        parseSize Crossref  = "bib2"

