module Compiler.Parser.Types (
  Expr (..),
  Stmt (..),
  Func (..),
  Program (..),
  Parser,
  UnaryOperator (..),
  Factor (..),
  BinaryOperator (..),
  ParseError (..),
)
where

import qualified Compiler.Lexer.Types as LT (Span (..), Token (..))
import Control.Monad.State (StateT)

data UnaryOperator = Complement | Negate | Not deriving (Eq, Show)

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | BitAnd
  | BitOr
  | Xor
  | LeftShift
  | RightShift
  | And
  | Or
  | Equal
  | NotEqual
  | GreaterThan
  | GreaterOrEqual
  | LessThan
  | LessOrEqual
  deriving (Eq, Show, Ord)

data Expr = Factor Factor | Binary BinaryOperator Expr Expr deriving (Eq, Show)

data Factor = Lit Int | Unary UnaryOperator Factor | Expr Expr deriving (Eq, Show)

newtype Stmt = Return Expr deriving (Eq, Show)

data Func = Func {name :: String, body :: Stmt} deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

data ParseError
  = UnexpectedToken LT.Token String
  | UnexpectedEOF String
  | ParseError String
  deriving (Show, Eq)

type Parser a = StateT [(LT.Token, LT.Span)] (Either ParseError) a
