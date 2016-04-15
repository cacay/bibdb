-----------------------------------------------------------------------------
-- |
-- Module      : Parser.Parser
-- Description : Parser for bibdb files
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- This module provides a parser for bibdb files that is intended to be
-- used with an Alex generated parser.
-----------------------------------------------------------------------------
{
module Parser.Parser
  ( -- * Parsers
    Parser
  , Entry (..)
  , runParser
  , fileParser
  , tokenParser
    -- * Lexemes
  , Token
  , Lexeme
  , token
  ) where

import Control.Monad (liftM)

import Data.ByteString.Lazy.Char8 (ByteString)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..))

import Parser.Location ( SrcSpan, Located (..), mergeLocated
                       , Loc, makeLoc, unLoc
                       )
import Parser.Token
import Parser.Lexer
import Reference
}

%name fileParser1 File
%name tokenParser1 Tokens

%monad { Alex }
%lexer { lexer } { Lexeme _ TEof }
%tokentype { Lexeme }
%error { parseError }


%token
  -- Keywords
  as           { Lexeme _ TAs }
  ':'          { Lexeme _ TColon }
  stype        { Lexeme _ (TType _) }
  ident        { Lexeme _ (TIdent _) }


%%

{--------------------------------------------------------------------------
    Helper production rules
--------------------------------------------------------------------------}

-- An optional production
Opt(p) : p               { Just $1 }
       | {- empty -}     { Nothing }

-- Empty production
Empty : {- empty -}      { () }

-- A possibly empty list of 'p's separated by 'sep's
ListSep(p, sep) : ListSep1(p, sep)    { $1 }
                | {- empty -}         { [] }

-- A list of 'p's separated by 'sep's
ListSep1(p, sep) : ListSep1R(p, sep)  { reverse $1 }

-- A list of 'p's separated by 'sep's in reverse order
ListSep1R(p, sep) : ListSep1R(p, sep) sep p  { $3 : $1 }
                  | p                        { [$1] }

-- A list of 'p's with no separators
List(p) : ListSep(p, Empty)                 { $1 }


{--------------------------------------------------------------------------
  Token Parser
--------------------------------------------------------------------------}


Tokens :: { [Lexeme] }
Tokens : List(Token) { $1 }

Token :: { Lexeme }
Token : stype        { $1 }
      | ident        { $1 }
      | ':'          { $1 }
      | as           { $1 }


{--------------------------------------------------------------------------
  Entry Parser
--------------------------------------------------------------------------}

Entry :: { Entry }
Entry : Source              { Entry (location $1) (fst $ unLoc $1) (snd $ unLoc $1) }
      | Source as Ident     { Entry (mergeLocated $1 $3) (fst $ unLoc $1) (unLoc $3) }


Source :: { Loc (Source, RefIdent) }
Source : stype ':' Key      { % parseSource (mergeLocated $1 $3) $1 (unLoc $3) }

Key :: { Loc SourceKey }
Key : ident                 { makeLoc (location $1) $ SourceKey $ (\(TIdent id) -> id) (token $1) }

Ident :: { Loc RefIdent }
Ident : ident               { makeLoc (location $1) $ RefIdent $ (\(TIdent id) -> id) (token $1) }


{--------------------------------------------------------------------------
    Top level
--------------------------------------------------------------------------}

File :: { [Entry] }
File : List(Entry)    { $1 }


{

{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}

-- | A monadic parser
type Parser t = Alex t

-- | Run a 'Parser' on the given 'ByteString'
runParser :: ByteString -> Parser t -> Either String t
runParser = runAlex

-- | Parser for a bibdb file
fileParser :: FilePath -> Parser [Entry]
fileParser path = setSrcFile path >> fileParser1

-- | Parse tokens only and return a list of lexemes. Mainly used for debugging.
tokenParser :: FilePath -> Parser [Lexeme]
tokenParser path = setSrcFile path >> tokenParser1


{--------------------------------------------------------------------------
  Entry
--------------------------------------------------------------------------}

data Entry = Entry
  { entryLoc    :: SrcSpan
  , entrySource :: Source
  , entryIdent  :: RefIdent
  }


instance Located Entry where
  location (Entry loc _ _) = loc

instance Pretty Entry where
  pPrint (Entry _ s id) = pPrint s <+> text "as" <+> pPrint id


{--------------------------------------------------------------------------
  Parser Helpers
--------------------------------------------------------------------------}

parseSource :: SrcSpan -> Lexeme -> SourceKey -> Alex (Loc (Source, RefIdent))
parseSource loc (Lexeme _ (TType t)) key = do
  t' <- parseType t
  let ident = t ++ ":" ++ (\(SourceKey k) -> k) key
  return $ makeLoc loc (Source t' key, RefIdent ident)
parseSource loc _ key = undefined

parseType :: String -> Alex SourceType
parseType "DOI" = return Doi
parseType "doi" = return Doi
parseType "arXiv" = return ArXiv
parseType "DBLP" = return Dblp
parseType "dblp" = return Dblp
parseType "CiteSeerX" = return CiteSeerX
parseType "HAL" = return Hal
parseType "hal" = return Hal
parseType "inria" = return Inria
parseType t = lexError $ "invalid source type" ++ t


{--------------------------------------------------------------------------
  Happy related
--------------------------------------------------------------------------}

parseError :: Lexeme -> Alex a
parseError l = lexError $ "parse error on " ++ show (token l)

}

