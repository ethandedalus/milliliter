{-# LANGUAGE LambdaCase #-}

module Compiler.Codegen.InstructionFixup (lowerProgram, lowerInstruction, lowerInstructions, lowerFunc, instructionFixup) where

import Compiler.Stage (Stage)

import Compiler.Codegen.Types (
  BinaryOperator (..),
  Func (..),
  Instruction (..),
  Operand (..),
  Program (..),
  Register (..),
 )

lowerInstruction :: Instruction -> [Instruction]
lowerInstruction = \case
  (Mov lhs@(Stack _) rhs@(Stack _)) -> [Mov lhs (Register R10), Mov (Register R10) rhs]
  (IDiv v@(Imm _)) -> [Mov v (Register R10), IDiv (Register R10)]
  (Binary Add lhs@(Stack _) rhs@(Stack _)) -> [Mov lhs (Register R10), Binary Add (Register R10) rhs]
  (Binary Sub lhs@(Stack _) rhs@(Stack _)) -> [Mov lhs (Register R10), Binary Sub (Register R10) rhs]
  (Binary Mul lhs rhs@(Stack _)) -> [Mov rhs (Register R11), Binary Mul lhs (Register R11), Mov (Register R11) rhs]
  other -> [other]

lowerInstructions :: [Instruction] -> [Instruction]
lowerInstructions = concatMap lowerInstruction

lowerFunc :: Func -> Func
lowerFunc (Func name instructions stackAlloc) = Func name (lowerInstructions instructions) stackAlloc

lowerProgram :: Program -> Program
lowerProgram (Program func) = Program (lowerFunc func)

instructionFixup :: (a -> b) -> Stage a b
instructionFixup f x = pure (f x)
