module Compiler.Stage where

import Compiler.Error(CompileError)

type Stage a b = a -> Either CompileError b
