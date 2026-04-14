{-# LANGUAGE LambdaCase #-}

module Compiler.Codegen.PseudoRegisters where

import Compiler.Codegen.Types (
  BinaryOperator,
  Func (..),
  Instruction (..),
  Operand (..),
  Program (..),
  PseudoRegistersPassState (..),
  Transform,
  currentOffset,
  stackOffsets,
 )
import qualified Compiler.Error as CE (CompileError (CodegenError))
import Compiler.Stage (Stage)
import Control.Lens (use, uses, (%=), (.=))
import Control.Monad.State (evalStateT, get)
import Data.Bifunctor (first)
import qualified Data.Map as Map (insert, lookup)

lowerVal :: Operand -> Transform PseudoRegistersPassState Operand
lowerVal = \case
  (Pseudo ident) -> do
    offsets <- use stackOffsets
    case Map.lookup ident offsets of
      Just v -> return $ Stack v
      _ -> do
        stackLocation <- uses currentOffset (subtract 4)
        stackOffsets %= Map.insert ident stackLocation
        currentOffset .= stackLocation
        return $ Stack stackLocation
  other -> return other

lowerMov :: Operand -> Operand -> Transform PseudoRegistersPassState Instruction
lowerMov lhs rhs = do
  lhs' <- lowerVal lhs
  rhs' <- lowerVal rhs
  return $ Mov lhs' rhs'

lowerBinary :: BinaryOperator -> Operand -> Operand -> Transform PseudoRegistersPassState Instruction
lowerBinary op lhs rhs = do
  lhs' <- lowerVal lhs
  rhs' <- lowerVal rhs
  return $ Binary op lhs' rhs'

lowerCmp :: Operand -> Operand -> Transform PseudoRegistersPassState Instruction
lowerCmp lhs rhs = do
  lhs' <- lowerVal lhs
  rhs' <- lowerVal rhs
  return $ Cmp lhs' rhs'

lowerInstruction :: Instruction -> Transform PseudoRegistersPassState Instruction
lowerInstruction = \case
  (Mov lhs rhs) -> lowerMov lhs rhs
  (Unary operator operand) -> Unary operator <$> lowerVal operand
  (Binary operator lhs rhs) -> lowerBinary operator lhs rhs
  (IDiv operand) -> IDiv <$> lowerVal operand
  (Cmp lhs rhs) -> lowerCmp lhs rhs
  (SetCC code operand) -> SetCC code <$> lowerVal operand
  other -> return other

lowerInstructions :: [Instruction] -> Transform PseudoRegistersPassState [Instruction]
lowerInstructions = mapM lowerInstruction

lowerFunc :: Func -> Transform PseudoRegistersPassState Func
lowerFunc (Func name instructions _) = do
  instructions' <- lowerInstructions instructions
  PseudoRegistersPassState _ current <- get
  return $ Func name instructions' (-current)

lowerProgram :: Program -> Transform PseudoRegistersPassState Program
lowerProgram (Program func) = do
  func' <- lowerFunc func
  return $ Program func'

lower :: (i -> Transform PseudoRegistersPassState a) -> Stage i a
lower l x = first CE.CodegenError $ evalStateT (l x) (PseudoRegistersPassState mempty 0)
