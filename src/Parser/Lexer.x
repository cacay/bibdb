-----------------------------------------------------------------------------
-- |
-- Module      : Parser.Lexer
-- Description : Lexer for bibdb files
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
{
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Parser.Lexer
  ( -- * The main lexing function
    lexer
    -- * Alex related
  , Alex (..), AlexReturn (..), runAlex, alexScanUser
  , lexError
    -- * Source location
  , setSrcFile, getSrcLoc
  ) where

import Control.Monad (ap, liftM, when)
import Control.Monad.State (MonadState (..), gets, put, modify)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Int (Int64)

import Lens.Micro
import Lens.Micro.Mtl
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..))

import Parser.AlexUserState
import Parser.Location hiding (srcFile)
import Parser.Token (Token (..), Lexeme (..))
}

%wrapper "monadUserState-bytestring"

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$newline     = [\n\r\f]
$space       = [\ ]
$tab         = \t

$upper       = [ A-Z ]
$lower       = [ a-z ]
$alpha       = [ $upper $lower ]

$white_no_nl = [ $white # $newline ]
$graphic     = [ $printable # $white ]
$comment     = [ $printable # $newline ]

$valid = $graphic


-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

@stype     = $alpha+
@ident     = $valid+


-- -----------------------------------------------------------------------------
-- Alex "Lexing"

tokens :-

-- States:
--   - 0       : Unused since 0 is a non-descriptive name
--   - stype   : Lexing a reference type
--   - ident   : Lexing an identifier
--   - comment : Lexing a multi-line comment

-- We do not use the 0 state since it is non-descriptive
<0> ()         { just $ switchTo stype }


-- Ignore white space
<stype, ident> $white         ;


-- Comments
<stype, ident> [\-]{2,} $comment*   ;

<stype, ident, comment> "{-"        { just $ pushCode comment }
<comment>    "-}"                   { just $ popCode }
<comment>    $comment               ;
<comment>    $newline               ;


-- Keywords
<stype> as          { mkTok (const TAs) `also` switchTo ident }
<stype> :           { mkTok (const TColon) `also` switchTo ident }

-- Types and identifiers
<stype>  @stype      { mkTok TType }
<ident>  @ident     { mkTok TIdent `also` switchTo stype }

{

-- -----------------------------------------------------------------------------
-- Alex "Types and instances"

type StartCode = Int

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure = return
  (<*>) = ap

instance MonadState AlexState Alex where
  get   = Alex $ \s -> Right (s, s)
  put s = Alex $ \_ -> Right (s, ())


-- -----------------------------------------------------------------------------
-- Alex "User state"

-- | A lens that can be used to get, set, and modify the user state
userState :: Lens' AlexState AlexUserState
userState f s = (\ust -> s { alex_ust = ust}) <$> f (alex_ust s)


-- | Get source file name
getSrcFile :: Alex FilePath
getSrcFile = use (userState . srcFile)

-- | Set source file name
setSrcFile :: FilePath -> Alex ()
setSrcFile name = userState . srcFile .= name


-- -----------------------------------------------------------------------------
-- Alex "Lexing"

alexEOF :: Alex Lexeme
alexEOF = do
  code <- alexGetStartCode
  when (code == comment) $ lexError "unmatched comment"
  loc <- getSrcLoc
  return $ Lexeme (srcLocSpan loc) TEof


-- | Construct a 'Lexeme' given a way to construct a 'Token' from the
-- lexed string. This function automatically adds the source position.
mkTok :: (String -> Token) -> AlexAction' Lexeme
mkTok f = action (\s p -> return $ Lexeme p (f s))

-- | Return the current position in the input as a 'SrcLoc'
getSrcLoc :: Alex SrcLoc
getSrcLoc = do
  f <- getSrcFile
  AlexPn abs line col <- gets alex_pos
  return $ makeSrcLoc f abs line col

-- | Display an error message. Source position and extra information is
-- added automatically by the function.
lexError :: String -> Alex a
lexError msg = do
  pos <- getSrcLoc
  (_, c, input) <- alexGetInput
  alexError $ render $ vcat
      [ pPrint pos <> colon <+> text msg
      , text $ c : "<ERROR>"
      , if BS.null input
          then text "[end of file]"
          else text $ BS.unpack (BS.take 30 input) ++ "..."
      ]


-- -----------------------------------------------------------------------------
-- Alex "Startcode stack management"

-- | Push the current startcode on the stack and switch to a new startcode
alexPushStartCode :: StartCode -> Alex ()
alexPushStartCode s = do
  current <- alexGetStartCode
  state <- use (userState . prevStartCodes)
  alexSetState (s : current : state)

-- | Drop the current startcode and switch to the top startcode on the stack
alexPopStartCode :: Alex ()
alexPopStartCode = use (userState . prevStartCodes) >>= alexSetState

-- | Set the entire state of the lexer at once changing all startcodes.
-- Head of the list becomes the current startcode and the tail becomes
-- the stack of previous startcodes.
alexSetState :: [StartCode] -> Alex ()
alexSetState [] = lexError "No startcodes in the stack. Lexer state cannot be empty."
alexSetState (x : xs) = do
  userState . prevStartCodes .= xs
  alexSetStartCode x

-- | Get the entire state of the lexer.
alexGetState :: Alex ([StartCode])
alexGetState = do
  current <- alexGetStartCode
  state <- use (userState . prevStartCodes)
  return (current : state)

-- | Shorthand for 'alexPushStartCode'
pushCode :: StartCode -> Alex ()
pushCode = alexPushStartCode

-- | ShortHand for 'alexPopStartCode'
popCode :: Alex ()
popCode = alexPopStartCode


-- | More informative name for 'begin'
switchTo :: StartCode -> Alex ()
switchTo = alexSetStartCode


-- -----------------------------------------------------------------------------
-- Alex "Useful combinators for actions"

-- | Nicer interface for Alex actions
type Action result = String -> SrcSpan -> Alex result

-- | Alex uses Int in its native definition of AlexAction even though it
-- internally expects an Int64
type AlexAction' result = AlexInput -> Int64 -> Alex result

-- | Nicer interface for Alex actions
action :: Action result -> AlexAction' result
action act (_, _, input) len = do
  span <- return makeSrcSpanLengthEnd `ap` getSrcLoc `ap` return (fromIntegral len)
  let str = BS.unpack $ BS.take len input
  act str span

-- | Execute the Alex monad ignoring the current lexeme
-- just :: Alex a -> AlexAction result
just m _ _ = m >> alexMonadScan

-- | Execute the monad and return the action
also :: AlexAction' result -> Alex a -> AlexAction' result
(act `also` m) input len = m >> act input len


-- -----------------------------------------------------------------------------
-- Alex "Running the lexer"

-- | Thread with Happy
lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
