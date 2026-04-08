{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser.Types (
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

import qualified Compiler.Types as CT (Literal (..))
import Control.Monad.State (StateT)
import qualified Lexer.Types as LT (Span (..), Token (..))
import Parser.Errors (ParseError (..))

data UnaryOperator = Complement | Negate deriving (Eq, Show)

data BinaryOperator = Add | Sub | Mul | Div | Mod deriving (Eq, Show, Ord)

data Expr = Factor Factor | Binary BinaryOperator Expr Expr deriving (Eq, Show)

data Factor = Lit CT.Literal | Unary UnaryOperator Factor | Expr Expr deriving (Eq, Show)

newtype Stmt = Return Expr deriving (Eq, Show)

data Func = Func {name :: String, body :: Stmt} deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

type Parser a = StateT [(LT.Token, LT.Span)] (Either ParseError) a
