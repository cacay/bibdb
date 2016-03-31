-----------------------------------------------------------------------------
-- |
-- Module      : Reference
-- Description : Data and functions related to references
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Reference
  ( Source (..)
  , SourceType (..)
  , SourceKey (..)
  , RefIdent (..)
  , BibTeX (..)
  , parseBibTeX
  , renameBib
  , crossrefs
  ) where

import Data.Char (toLower)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import qualified Text.BibTeX.Entry as Entry
import Text.BibTeX.Parse (skippingLeadingSpace, file)
import Text.Parsec.Prim (parse)


data Source = Source
  { sourceType :: SourceType
  , sourceKey  :: SourceKey
  }
  deriving (Eq, Ord)

data SourceType = Dblp | CiteSeerX | Doi
  deriving (Eq, Ord)

newtype SourceKey = SourceKey String
  deriving (Eq, Ord)


newtype RefIdent = RefIdent String
  deriving (Eq, Ord)


data BibTeX = BibTeX
  { bibType   :: String
  , bibIdent  :: RefIdent
  , bibFields :: [(String, String)]
  }


renameBib :: RefIdent -> BibTeX -> BibTeX
renameBib new bib = bib {bibIdent = new}

crossrefs :: BibTeX -> [RefIdent]
crossrefs (BibTeX _ _ fields) =
  map (RefIdent . snd) (filter isCrossref fields)
  where
    isCrossref :: (String, String) -> Bool
    isCrossref (name, _) = map toLower name == "crossref"


----------------------------------------------------------------------------
-- * Parsing
----------------------------------------------------------------------------

parseBibTeX :: String -> Either String [BibTeX]
parseBibTeX s = case parse (skippingLeadingSpace file) "" s of
  Left err -> Left (show err ++ "\n" ++ s)
  Right entries -> Right (map convert entries)
  where
    convert :: Entry.T -> BibTeX
    convert (Entry.Cons t id fields) = BibTeX t (RefIdent id) fields


----------------------------------------------------------------------------
-- * Printing
----------------------------------------------------------------------------

instance Pretty Source where
  pPrint (Source t key) = pPrint t <> colon <> pPrint key

instance Pretty SourceType where
  pPrint Dblp = text "DBLP"
  pPrint CiteSeerX = text "CiteSeerX"
  pPrint Doi = text "DOI"

instance Pretty SourceKey where
  pPrint (SourceKey key) = text key


instance Pretty RefIdent where
  pPrint (RefIdent id) = text id


instance Pretty BibTeX where
  pPrint (BibTeX t id fields) =
    hang (char '@' <> text t <> lbrace <> pPrint id <> comma) 2
      (vcat $ punctuate comma $ map printField fields)
    $+$ (rbrace <> char '\n')
    where
      printField :: (String, String) -> Doc
      printField (name, value) = text name <+> equals <+> braces (text value)



----------------------------------------------------------------------------
-- * Showing
----------------------------------------------------------------------------

instance Show Source where
  show = prettyShow

instance Show SourceType where
  show = prettyShow

instance Show SourceKey where
  show = prettyShow


instance Show RefIdent where
  show = prettyShow


instance Show BibTeX where
  show = prettyShow

