module CompileError where

import Codegen.Types (CodegenError)
import IR.Errors (IRError)
import Lexer.Types (LexError)
import Parser.Errors (ParseError)

data CompileError
  = LexError LexError
  | ParseError ParseError
  | IRError IRError
  | CodegenError CodegenError
  deriving (Eq, Show)
