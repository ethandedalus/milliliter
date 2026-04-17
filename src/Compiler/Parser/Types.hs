{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Compiler.Parser.Types where

import qualified Compiler.Lexer.Types as LT (Span, Token)
import Control.Monad.State (StateT)

data ParseError
  = UnexpectedToken LT.Token String
  | UnexpectedEOF String
  | ParseError String
  deriving (Show, Eq)

type Parser a = StateT [(LT.Token, LT.Span)] (Either ParseError) a
