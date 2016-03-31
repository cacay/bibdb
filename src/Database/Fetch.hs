
-----------------------------------------------------------------------------
-- |
-- Module      : Database.Fetch
-- Description : Communication protocols for databases
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Database.Fetch (fetch, fetchAll) where

import Args (BibSize)
import Reference

import qualified Database.Dblp as Dblp

import Utility.Except


fetch :: BibSize -> Source -> Exception (RefIdent, [BibTeX])
fetch size source@(Source t key) = do
  liftIO $ putStrLn $ "Fetching " ++ show source
  case t of
    Dblp -> Dblp.fetch size key
    _ -> undefined


fetchAll :: BibSize -> [Source] -> Exception ([RefIdent], [BibTeX])
fetchAll size srcs = do
  results <- mapM (fetch size) srcs
  let (idents, bibs) = unzip results
  return (idents, concat bibs)

