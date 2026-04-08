module Compiler.Types (Literal (..), CompileError (..)) where

data Literal
  = LiteralInt Int
  | LiteralFloat Float
  | LiteralString String
  | LiteralChar Char
  | LiteralBool Bool
  deriving (Eq, Show)

data CompileError
  = LexError String
  | ParseError String
  | IRError String
  | CodegenError String
  deriving (Eq, Show)
