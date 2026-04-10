module Compiler.Codegen.ASM (
  lowerVal,
  lowerInstruction,
  lowerInstructions,
  lowerFunc,
  lowerProgram,
  lower,
)
where

import Compiler.Class (ConversionError (..), from, tryFrom)
import Compiler.Codegen.Types (
  BinaryOperator (..),
  CodegenError (..),
  Func (..),
  Instruction (..),
  Operand (..),
  Program (..),
  Register (..),
 )
import qualified Compiler.Error as CE (CompileError (CodegenError))
import qualified Compiler.IR.Types as IR (BinaryOperator (..), Func (..), Instruction (..), Program (..), Val (..))
import Compiler.Stage (Stage)
import Compiler.Types (Literal (LiteralInt))
import Control.Monad.Except (liftEither)
import Data.Bifunctor (first)

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
lowerInstruction (IR.Binary IR.Div src1 src2 dst) = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' (Register AX), CDQ, IDiv src2', Mov (Register AX) dst']
lowerInstruction (IR.Binary IR.Mod src1 src2 dst) = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' (Register AX), CDQ, IDiv src2', Mov (Register DX) dst']
lowerInstruction (IR.Binary IR.LeftShift src1 src2 dst) = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' dst', Mov src2' (Register CX), Binary LeftShift (Register CL) dst']
lowerInstruction (IR.Binary op src1 src2 dst) = do
  op' <- liftEither $ first ConvertBinaryOperator (tryFrom op :: Either ConversionError BinaryOperator)
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' dst', Binary op' src2' dst']

lowerInstructions :: [IR.Instruction] -> Either CodegenError [Instruction]
lowerInstructions instructions = concat <$> mapM lowerInstruction instructions

lowerFunc :: IR.Func -> Either CodegenError Func
lowerFunc (IR.Func name instructions) = do
  instructions' <- lowerInstructions instructions
  return (Func name instructions' 0)

lowerProgram :: IR.Program -> Either CodegenError Program
lowerProgram (IR.Program func) = Program <$> lowerFunc func

lower :: (i -> Either CodegenError a) -> Stage i a
lower f = first CE.CodegenError . f
