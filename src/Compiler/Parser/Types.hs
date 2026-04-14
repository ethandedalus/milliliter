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
  BlockItem (..),
  Decl (..),
)
where

import qualified Compiler.Lexer.Types as LT (Span (..), Token (..))
import Control.Monad.State (StateT)

data UnaryOperator
  = Complement
  | Negate
  | Not
  | PrefixIncrement
  | PostfixIncrement
  | PrefixDecrement
  | PostfixDecrement
  deriving (Eq, Show)

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
  | Assignment
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | ShlAssign
  | ShrAssign
  | AndAssign
  | OrAssign
  | XorAssign
  deriving (Eq, Show, Ord)

data Expr
  = Var String
  | Factor Factor
  | Binary BinaryOperator Expr Expr
  | Assign Expr Expr
  | CompoundAssign BinaryOperator Expr Expr
  deriving (Eq, Show)

data Factor = Lit Int | Unary UnaryOperator Factor | Expr Expr | Ident String deriving (Eq, Show)

data Stmt
  = Return Expr
  | ExprS Expr
  | Null
  deriving (Eq, Show)

data Decl = Decl String (Maybe Expr) deriving (Eq, Show)

data BlockItem = S Stmt | D Decl deriving (Eq, Show)

data Func = Func {name :: String, body :: [BlockItem]} deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

data ParseError
  = UnexpectedToken LT.Token String
  | UnexpectedEOF String
  | ParseError String
  deriving (Show, Eq)

type Parser a = StateT [(LT.Token, LT.Span)] (Either ParseError) a
