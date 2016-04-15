-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : Main file for bibdb bibliography manager
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Main (main) where

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (replaceExtension)

import qualified Data.ByteString.Lazy as BS
import Data.List (groupBy, sort, sortOn)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..))

import qualified Options.Applicative as Options
import qualified Args

import Parser.Location (Located (..), spanSLine)
import qualified Parser.Parser as Parser
import Database.Fetch (fetchAll)
import Bibliography (bibliography)

import Utility.Except


main :: IO ()
main = do
  job <- Options.execParser opts
  res <- runExceptT (runJob job)
  case res of
    Left err -> do putStrLn err; exitFailure
    Right () -> exitSuccess
  where
    opts :: Options.ParserInfo Args.Job
    opts = Options.info (Options.helper <*> Args.jobParser)
      ( Options.fullDesc
      Options.<> Options.progDesc "Gather references specified in FILE"
      Options.<> Options.header "Bibdb bibliography manager"
      )

runJob :: Args.Job -> Exception ()
runJob job = do
  bib <- liftIO $ BS.readFile (Args.jobSource job)

  when (Args.jobStopAt job == Args.Lexing) $ do
    tokens <- liftEither $ Parser.runParser bib $
                Parser.tokenParser (Args.jobSource job)
    save "lexing" tokens
    printAndExit tokens

  entries <- liftEither $ Parser.runParser bib $
               Parser.fileParser $ Args.jobSource job
  save "parsing" entries
  when (Args.jobStopAt job == Args.Parsing) $ printAndExit entries

  checkDuplicatesOn Parser.entryIdent entries

  (ids, database) <- fetchAll (Args.jobBibSize job) (map Parser.entrySource entries)
  save "gather" database
  when (Args.jobStopAt job == Args.Downloading) $ printAndExit database

  let renames = zip ids (map Parser.entryIdent entries)
  let final = bibliography renames database
  let msg = "Automatically generated file. DO NOT MODIFY!\n"
  liftIO $ writeFile outputFile (msg ++ "\n" ++ renderList final)

  where
    outputFile :: FilePath
    outputFile = fromMaybe def (Args.jobOutput job)
      where def = replaceExtension (Args.jobSource job) "bib"

    save :: Pretty a => String -> [a] -> ExceptT String IO ()
    save extension es | Args.jobKeepIntermediate job =
      let out = replaceExtension outputFile extension
      in liftIO $ writeFile out (renderList es)
    save _ _ = return ()

    printAndExit :: Pretty a => [a] -> ExceptT String IO ()
    printAndExit es = do
      liftIO $ putStrLn $ renderList es
      liftIO exitSuccess

    renderList :: Pretty a => [a] -> String
    renderList = render . vcat . map pPrint


-- | Check that all elements of a list are distinct. Uses the given
-- function to extract the key to compare on.
checkDuplicatesOn :: forall key e . (Ord key, Pretty key, Located e)
                  => (e -> key)     -- ^ Extract a key to compare on
                  -> [e]            -- ^ List of inputs
                  -> Exception ()
checkDuplicatesOn key es = case map duplicateError (duplicates es) of
  [] -> return ()
  errs -> throwError $ render $ vcat errs
  where
    -- Find duplicates
    duplicates :: [e] -> [[e]]
    duplicates = filter ((>= 2) . length) . groupBy ((==) `on` key) . sortOn key

    -- Generate an error message for a list of duplicates
    duplicateError :: [e] -> Doc
    duplicateError ds@(d : _) = text "Multiple definitions of" <+> pPrint (key d)
      <+> text "at lines" <+> summarizeLocations ds
    duplicateError [] = error "impossible"

    summarizeLocations :: [e] -> Doc
    summarizeLocations = hsep . punctuate comma . map int . sort . map line

    line :: Located a => a -> Int
    line = spanSLine . location

