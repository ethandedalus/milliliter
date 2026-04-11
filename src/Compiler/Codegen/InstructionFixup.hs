{-# LANGUAGE LambdaCase #-}

module Compiler.Codegen.InstructionFixup (lowerProgram, lowerInstruction, lowerInstructions, lowerFunc, lower) where

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
  (Binary op lhs@(Stack _) rhs@(Stack _)) | op `elem` [BitAnd, BitOr, Xor] -> [Mov lhs (Register R12), Binary op (Register R12) rhs]
  (Binary Mul lhs rhs@(Stack _)) -> [Mov rhs (Register R11), Binary Mul lhs (Register R11), Mov (Register R11) rhs]
  (Cmp lhs rhs@(Imm _)) -> [Mov rhs (Register R11), Cmp lhs (Register R11)]
  (Cmp lhs@(Stack _) rhs@(Stack _)) -> concatMap lowerInstruction [Mov lhs (Register R10), Cmp (Register R10) rhs]
  other -> [other]

lowerInstructions :: [Instruction] -> [Instruction]
lowerInstructions = concatMap lowerInstruction

lowerFunc :: Func -> Func
lowerFunc (Func name instructions stackAlloc) = Func name (lowerInstructions instructions) stackAlloc

lowerProgram :: Program -> Program
lowerProgram (Program func) = Program (lowerFunc func)

lower :: (a -> b) -> Stage a b
lower f x = pure (f x)
