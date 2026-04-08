module Codegen.ASM (
  lowerVal,
  lowerInstruction,
  lowerInstructions,
  lowerFunc,
  lower,
)
where

import Codegen.Types (
  CodegenError (..),
  Func (..),
  Instruction (..),
  Operand (..),
  Program (..),
  Register (..),
 )
import Compiler.Class (from)
import Compiler.Types (Literal (LiteralInt))
import qualified IR.Types as IR (Func (..), Instruction (..), Program (..), Val (..))

lowerVal :: IR.Val -> Either CodegenError Operand
lowerVal (IR.Lit (LiteralInt v)) = Right (Imm v)
lowerVal (IR.Var ident) = Right (Pseudo ident)
lowerVal _ = Left (CodegenError "unsupported")

lowerInstruction :: IR.Instruction -> Either CodegenError [Instruction]
lowerInstruction (IR.Return val) = do
  operand <- lowerVal val
  return [Mov operand (Register AX), Ret]
lowerInstruction (IR.Unary op src dst) = do
  src' <- lowerVal src
  dst' <- lowerVal dst
  return [Mov src' dst', Unary (from op) dst']

lowerInstructions :: [IR.Instruction] -> Either CodegenError [Instruction]
lowerInstructions instructions = concat <$> mapM lowerInstruction instructions

lowerFunc :: IR.Func -> Either CodegenError Func
lowerFunc (IR.Func name instructions) = do
  instructions' <- lowerInstructions instructions
  return (Func name instructions' 0)

lower :: IR.Program -> Either CodegenError Program
lower (IR.Program func) = Program <$> lowerFunc func
