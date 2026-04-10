{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Parser.Types (
  Expr (..),
  Stmt (..),
  Func (..),
  Program (..),
  Parser,
  UnaryOperator (..),
  Factor (..),
  BinaryOperator (..),
)
where

import qualified Compiler.Lexer.Types as LT (Span (..), Token (..))
import Compiler.Parser.Errors (ParseError (..))
import qualified Compiler.Types as CT (Literal (..))
import Control.Monad.State (StateT)

data UnaryOperator = Complement | Negate deriving (Eq, Show)

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor
  | LeftShift
  | RightShift
  deriving (Eq, Show, Ord)

data Expr = Factor Factor | Binary BinaryOperator Expr Expr deriving (Eq, Show)

data Factor = Lit CT.Literal | Unary UnaryOperator Factor | Expr Expr deriving (Eq, Show)

newtype Stmt = Return Expr deriving (Eq, Show)

data Func = Func {name :: String, body :: Stmt} deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

type Parser a = StateT [(LT.Token, LT.Span)] (Either ParseError) a
