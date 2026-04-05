{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Types (Expr (..), Stmt (..), Func (..), Program (..), Parser, AST (..), UnaryOperator (..)) where

import qualified Compiler.Types as CT (Literal (..))
import Control.Monad.State (StateT)
import qualified Lexer.Types as LT (Span (..), Token (..))
import Parser.Errors (ParseError (..))

data UnaryOperator = Complement | Negate deriving (Eq, Show)

data Expr = Lit CT.Literal | Unary UnaryOperator Expr deriving (Eq, Show)

newtype Stmt = Return Expr deriving (Eq, Show)

data Func = Func {name :: String, body :: Stmt} deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

type Parser a = StateT [(LT.Token, LT.Span)] (Either ParseError) a

data AST = ExprNode Expr | StmtNode Stmt | FuncNode Func | ProgramNode Program deriving (Show, Eq)
