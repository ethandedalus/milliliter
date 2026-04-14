module Compiler.Error where

import Compiler.Codegen.Types (CodegenError)
import Compiler.IR.Types (IRError)
import Compiler.Lexer.Types (LexError)
import Compiler.Parser.Types (ParseError)
import Compiler.SemanticAnalysis.Types (SemanticAnalysisError)

data CompileError
  = LexError LexError
  | ParseError ParseError
  | SemanticAnalysisError SemanticAnalysisError
  | IRError IRError
  | CodegenError CodegenError
  | CompileError String
  deriving (Eq, Show)
