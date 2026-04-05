module Compiler.Types where

data Literal
  = LiteralInt Int
  | LiteralFloat Float
  | LiteralString String
  | LiteralChar Char
  | LiteralBool Bool
  deriving (Eq, Show)
