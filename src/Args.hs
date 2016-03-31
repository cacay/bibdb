-----------------------------------------------------------------------------
-- |
-- Module      : Args
-- Description : Argument and option parsing
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Args
  ( Job (..)
  , BibSize (..)
  , Stage (..)
  , jobParser
  ) where

import Options.Applicative

-- | A preprocessor job
data Job = Job
  { jobSource           :: FilePath
  , jobOutput           :: Maybe FilePath
  , jobBibSize          :: BibSize
  , jobStopAt           :: Stage
  , jobKeepIntermediate :: Bool
  }

-- | Processing stage
data Stage = Lexing | Parsing | Downloading | Output
  deriving (Eq, Ord, Show)

-- | Bibliography size
data BibSize = Condensed | Standard | Crossref
  deriving (Eq, Ord, Show)


-- | Parse a job
jobParser :: Parser Job
jobParser = Job
  <$> argument str (metavar "FILE")
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Place the output into <FILE>"
      ))
  <*> bibSizeParser
  <*> stageParser
  <*> switch
      ( long "save-temps"
     <> short 't'
     <> help "Save intermediate forms"
      )

bibSizeParser :: Parser BibSize
bibSizeParser =
      flag Standard Condensed
      ( long "condensed"
     <> short 's'
     <> help "Generate condensed bibliography"
      )
  <|> flag Standard Crossref
      ( long "crossref"
     <> short 'c'
     <> help "Generate full bibliography with cross-references"
      )

stageParser :: Parser Stage
stageParser =
      flag completeRun Lexing
      ( long "lex"
     <> help "Lex only (output a token stream)"
      )
  <|> flag completeRun Parsing
      ( long "parse"
     <> help "Parse only (act as a pretty printer)"
      )
  <|> flag completeRun Downloading
      ( long "download"
     <> help "Download references but do not generate output"
      )
  <|> flag completeRun Output
      ( long "complete-run"
     <> help "Download references and produce output"
      )
  where
    completeRun :: Stage
    completeRun = Output



