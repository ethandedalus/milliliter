module Compiler.Error where

import Compiler.Codegen.Types (CodegenError)
import Compiler.IR.Errors (IRError)
import Compiler.Lexer.Types (LexError)
import Compiler.Parser.Errors (ParseError)

data CompileError
  = LexError LexError
  | ParseError ParseError
  | IRError IRError
  | CodegenError CodegenError
  | CompileError String
  deriving (Eq, Show)
