module Compiler.IR.Errors (IRError (..)) where

newtype IRError = IRError String deriving (Show, Eq)
