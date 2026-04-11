module Compiler.Types (CompileError (..)) where

data CompileError
  = LexError String
  | ParseError String
  | IRError String
  | CodegenError String
  deriving (Eq, Show)
