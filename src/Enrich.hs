-----------------------------------------------------------------------------
-- |
-- Module      : Enrich
-- Description : Try to add missing entries to all references in a database
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Enrich (enrich) where

import Control.Arrow ((&&&))
import Control.Concurrent.Async
import Control.Monad (mapM, foldM)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO (..))

import Data.Char (toLower)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map

import Args (BibSize (..))
import Reference
import qualified Database.Fetch as Fetch


type Database =
    Map.Map RefIdent BibTeX


data Metadata
    = NotAsked
    | Failure String
    | Success [BibTeX]


enrich :: (MonadIO m) => BibSize -> [String] -> [BibTeX] -> [(RefIdent, Source)] -> m [BibTeX]
enrich bibSize entries bibs references = do
    newBibs <- liftIO $ mapConcurrently (uncurry $ enrichIdent bibSize database entries) references
    let newDatabase = foldr (\bib -> Map.insert (bibIdent bib) bib) database newBibs
    return $ Map.elems newDatabase
    where
        database :: Database
        database = Map.fromList $ map (bibIdent &&& id) bibs


enrichIdent :: (MonadIO m) => BibSize -> Database -> [String] -> RefIdent -> Source -> m BibTeX
enrichIdent bibSize database entries ident source | Just bib <- Map.lookup ident database =
    let
        metadata :: Metadata
        metadata =
            case (bibSize, sourceType source) of
                (Condensed, Dblp) ->
                    -- We can get more information in this case
                    NotAsked
                _ ->
                    -- Fetching will just return the same data
                    Success $ bib : lookupCrossrefs database bib

        addAll :: (MonadState Metadata m, MonadIO m) => m BibTeX
        addAll =
            foldM (\bib entry -> enrichBibTeX database entry bib source) bib entries
    in
        evalStateT addAll metadata
enrichIdent bibSize database entries ident source | otherwise =
    error $ "Reference not in database: " ++ show source ++ "\nThis should never happen. Report a bug."


enrichBibTeX :: (MonadState Metadata m, MonadIO m) => Database -> String -> BibTeX -> Source -> m BibTeX
enrichBibTeX database entry bib source = do
    case any (Maybe.isJust . lookupField entry) (bib : lookupCrossrefs database bib) of
        True ->
            -- The entry is already there, no need to enrich
            return bib
        False -> do
            -- We need to enrich
            res <- runExceptT $
                enrichFromSource entry bib source `catchError` const (enrichFromMetadata entry bib source)
            case res of
                Left err -> do
                    liftIO $ putStrLn $ "Warning: cannot enrich " ++ show source ++ " with " ++ entry
                    return bib
                Right bib ->
                    return bib


-- | Enrich just by looking at the specified source
enrichFromSource :: MonadError String m => String -> BibTeX -> Source -> m BibTeX
enrichFromSource entry bib source =
    if sourceType source == Doi && map toLower entry == "doi"
        then do
            let SourceKey doi = sourceKey source
            return $ addField entry doi bib
        else
            throwError "Cannot enrich from source"


-- | Enrich by fetching a more comprehensive bibliography item from the server
-- (if necessariy and possible) and looking at the extended fields
enrichFromMetadata :: forall m . (MonadError String m, MonadState Metadata m, MonadIO m)
                   => String
                   -> BibTeX
                   -> Source
                   -> m BibTeX
enrichFromMetadata entry bib source = do
    metadata <- get
    case metadata of
        NotAsked ->
            let
                fetch :: m BibTeX
                fetch = do
                    (_, meta) <- Fetch.fetch Standard source
                    put (Success meta)
                    enrichFromMetadata entry bib source
            in
                catchError fetch $ \e -> do
                    put (Failure e)
                    throwError "Failed to fetch extra information"
        Failure e ->
            throwError "Failed to fetch extra information"
        Success meta ->
            case Maybe.mapMaybe (lookupField entry) meta of
                value : _ ->
                    return $ addField entry value bib
                _ ->
                    throwError "No information found"


lookupCrossrefs :: Database -> BibTeX -> [BibTeX]
lookupCrossrefs database bib =
    Maybe.mapMaybe (flip Map.lookup database) (Reference.crossrefs bib)
