
-----------------------------------------------------------------------------
-- |
-- Module      : Database.Fetch
-- Description : Communication protocols for databases
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.Fetch (fetch, fetchAll) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))

import Args (BibSize)
import Reference

import qualified Database.Doi as Doi
import qualified Database.Dblp as Dblp
import qualified Database.ArXiv as ArXiv
import qualified Database.Hal as Hal

import Utility.Except


fetch :: (MonadError String m, MonadIO m) => BibSize -> Source -> m (RefIdent, [BibTeX])
fetch size source@(Source t key) = do
  liftIO $ putStrLn $ "Fetching " ++ show source
  bibstr <- fetchString size key
  case parseBibTeX bibstr of
    Left err -> throwError $ "Error: cannot parse server response:\n" ++ err
    Right bibtex@(h : _) -> return (bibIdent h, bibtex)
    Right _ -> throwError $ "Error: empty response from server"
  where
    fetchString :: (MonadError String m, MonadIO m) => BibSize -> SourceKey -> m String
    fetchString = case t of
      Doi -> Doi.fetchString
      Dblp -> Dblp.fetchString
      ArXiv -> ArXiv.fetchString
      Hal -> Hal.fetchString
      Inria -> \size key -> Hal.fetchString size (inriaToHalKey key)
      _ -> undefined

    inriaToHalKey :: SourceKey -> SourceKey
    inriaToHalKey (SourceKey key) = SourceKey ("inria-" ++ key)


fetchAll :: (MonadError String m, MonadIO m) => BibSize -> [Source] -> m ([RefIdent], [BibTeX])
fetchAll size srcs = do
  results <- mapM (fetch size) srcs
  let (idents, bibs) = unzip results
  return (idents, concat bibs)

