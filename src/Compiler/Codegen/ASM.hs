module Compiler.Codegen.ASM (
  lowerVal,
  lowerInstruction,
  lowerInstructions,
  lowerFunc,
  lowerProgram,
  lower,
)
where

import Compiler.Class (from)
import Compiler.Codegen.Types (
  BinaryOperator (..),
  CodegenError (..),
  CondCode (..),
  Func (..),
  Instruction (..),
  Operand (..),
  Program (..),
  Register (..),
 )
import qualified Compiler.Error as CE (CompileError (CodegenError))
import qualified Compiler.IR.Types as IR (
  BinaryOperator (..),
  Func (..),
  Instruction (..),
  Program (..),
  UnaryOperator (..),
  Val (..),
 )
import Compiler.Stage (Stage)
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (first)

lowerVal :: IR.Val -> Either CodegenError Operand
lowerVal (IR.Lit v) = Right (Imm v)
lowerVal (IR.Var ident) = Right (Pseudo ident)

lowerCopy :: IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerCopy src dst = do
  src' <- lowerVal src
  dst' <- lowerVal dst
  return [Mov src' dst']

lowerReturn :: IR.Val -> Either CodegenError [Instruction]
lowerReturn src = lowerVal src >>= \operand -> return [Mov operand (Register AX), Ret]

lowerUnaryNot :: IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerUnaryNot src dst = do
  src' <- lowerVal src
  dst' <- lowerVal dst
  return [Cmp (Imm 0) src', Mov (Imm 0) dst', SetCC CondE dst']

lowerUnary :: IR.UnaryOperator -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerUnary op src dst = do
  src' <- lowerVal src
  dst' <- lowerVal dst
  return [Mov src' dst', Unary (from op) dst']

lowerDiv :: IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerDiv src1 src2 dst = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' (Register AX), CDQ, IDiv src2', Mov (Register AX) dst']

lowerMod :: IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerMod src1 src2 dst = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' (Register AX), CDQ, IDiv src2', Mov (Register DX) dst']

lowerLeftShift :: IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerLeftShift src1 src2 dst = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' dst', Mov src2' (Register CX), Binary LeftShift (Register CX) dst']

lowerRightShift :: IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerRightShift src1 src2 dst = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' dst', Mov src2' (Register CX), Binary RightShift (Register CX) dst']

lowerRelational :: CondCode -> IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerRelational cc src1 src2 dst = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Cmp src2' src1', Mov (Imm 0) dst', SetCC cc dst']

lowerJump :: CondCode -> IR.Val -> String -> Either CodegenError [Instruction]
lowerJump cc src target = do
  src' <- lowerVal src
  return [Cmp (Imm 0) src', JmpCC cc target]

lowerBinary :: BinaryOperator -> IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerBinary op src1 src2 dst = do
  src1' <- lowerVal src1
  src2' <- lowerVal src2
  dst' <- lowerVal dst
  return [Mov src1' dst', Binary op src2' dst']

lowerAdd, lowerSub, lowerMul, lowerBitAnd, lowerBitOr, lowerXor :: IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerAdd = lowerBinary Add
lowerSub = lowerBinary Sub
lowerMul = lowerBinary Mul
lowerBitAnd = lowerBinary BitAnd
lowerBitOr = lowerBinary BitOr
lowerXor = lowerBinary Xor

lowerG, lowerGE, lowerL, lowerLE, lowerE, lowerNE :: IR.Val -> IR.Val -> IR.Val -> Either CodegenError [Instruction]
lowerG = lowerRelational CondG
lowerGE = lowerRelational CondGE
lowerL = lowerRelational CondL
lowerLE = lowerRelational CondLE
lowerE = lowerRelational CondE
lowerNE = lowerRelational CondNE

-- | lowerInstruction lowers a three-address-code instruction to a list of assembly instruction
lowerInstruction :: IR.Instruction -> Either CodegenError [Instruction]
lowerInstruction (IR.Copy src dst) = lowerCopy src dst
lowerInstruction (IR.Return src) = lowerReturn src
-- unary

lowerInstruction (IR.Unary IR.Not src dst) = lowerUnaryNot src dst
lowerInstruction (IR.Unary op src dst) = lowerUnary op src dst
-- arithmetic

lowerInstruction (IR.Binary IR.Add src1 src2 dst) = lowerAdd src1 src2 dst
lowerInstruction (IR.Binary IR.Sub src1 src2 dst) = lowerSub src1 src2 dst
lowerInstruction (IR.Binary IR.Mul src1 src2 dst) = lowerMul src1 src2 dst
lowerInstruction (IR.Binary IR.Div src1 src2 dst) = lowerDiv src1 src2 dst
lowerInstruction (IR.Binary IR.Mod src1 src2 dst) = lowerMod src1 src2 dst
-- bitwise

lowerInstruction (IR.Binary IR.LeftShift src1 src2 dst) = lowerLeftShift src1 src2 dst
lowerInstruction (IR.Binary IR.RightShift src1 src2 dst) = lowerRightShift src1 src2 dst
lowerInstruction (IR.Binary IR.BitAnd src1 src2 dst) = lowerBitAnd src1 src2 dst
lowerInstruction (IR.Binary IR.BitOr src1 src2 dst) = lowerBitOr src1 src2 dst
lowerInstruction (IR.Binary IR.Xor src1 src2 dst) = lowerXor src1 src2 dst
-- relational/comparison

lowerInstruction (IR.Binary IR.GreaterThan src1 src2 dst) = lowerG src1 src2 dst
lowerInstruction (IR.Binary IR.GreaterOrEqual src1 src2 dst) = lowerGE src1 src2 dst
lowerInstruction (IR.Binary IR.LessThan src1 src2 dst) = lowerL src1 src2 dst
lowerInstruction (IR.Binary IR.LessOrEqual src1 src2 dst) = lowerLE src1 src2 dst
lowerInstruction (IR.Binary IR.Equal src1 src2 dst) = lowerE src1 src2 dst
lowerInstruction (IR.Binary IR.NotEqual src1 src2 dst) = lowerNE src1 src2 dst
lowerInstruction i@(IR.Binary{}) = throwError (UnlowerableInstruction i "ICE: this binary instruction is not lowerable")
-- jump

lowerInstruction (IR.JumpIfZero src target) = lowerJump CondE src target
lowerInstruction (IR.JumpIfNonZero src target) = lowerJump CondNE src target
lowerInstruction (IR.Label label) = return [Label label]
lowerInstruction (IR.Jump target) = return [Jmp target]

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
