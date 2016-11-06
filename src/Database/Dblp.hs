-----------------------------------------------------------------------------
-- |
-- Module      : Database.Dblp
-- Description : Communication protocols for dblp
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.Dblp (fetchString) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))

import Network.Curl.Download (openURIString)

import Args (BibSize (..))
import Reference

import Utility.Except


fetchString :: (MonadError String m, MonadIO m) => BibSize -> SourceKey -> m String
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

