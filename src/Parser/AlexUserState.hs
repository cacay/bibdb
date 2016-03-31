{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Parser.AlexUserState
-- Description : Lexer extra state
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Parser.AlexUserState
  ( -- * User state
    AlexUserState
  , alexInitUserState
    -- * Lenses
  , srcFile
  , prevStartCodes
  ) where

import Lens.Micro.TH (makeLenses)


type StartCode = Int


data AlexUserState = AlexUserState
  { _srcFile        :: FilePath
  , _prevStartCodes :: [StartCode]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { _srcFile        = "<no file>"
  , _prevStartCodes = []
  }


makeLenses ''AlexUserState

