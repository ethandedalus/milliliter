{-# LANGUAGE LambdaCase #-}

module Codegen.InstructionFixup (lower, lowerInstruction, lowerInstructions, lowerFunc) where

import Codegen.Types (Func (..), Instruction (..), Operand (..), Program (..), Register (..))

lowerInstruction :: Instruction -> [Instruction]
lowerInstruction = \case
  (Mov lhs@(Stack _) rhs@(Stack _)) -> [Mov lhs (Register R10), Mov (Register R10) rhs]
  other -> [other]

lowerInstructions :: [Instruction] -> [Instruction]
lowerInstructions = concatMap lowerInstruction

lowerFunc :: Func -> Func
lowerFunc (Func name instructions stackAlloc) = Func name (lowerInstructions instructions) stackAlloc

lower :: Program -> Program
lower (Program func) = Program (lowerFunc func)
