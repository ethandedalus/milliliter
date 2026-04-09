module Compiler.Codegen.PseudoRegisters where

import Compiler.Codegen.Types (
  Func (..),
  Instruction (..),
  Operand (..),
  Program (..),
  PseudoRegistersPassState (..),
  Transform,
 )
import qualified Compiler.Error as CE (CompileError (CodegenError))
import Compiler.Stage (Stage)
import Control.Monad.State (evalStateT, get, put)
import Data.Bifunctor (first)
import qualified Data.Map as Map (insert, lookup)

lowerVal :: Operand -> Transform PseudoRegistersPassState Operand
lowerVal op = do
  PseudoRegistersPassState offsets current <- get
  case op of
    (Pseudo ident) -> do
      case Map.lookup ident offsets of
        Just v -> return $ Stack v
        _ -> do
          let stackLocation = current - 4
          put $ PseudoRegistersPassState (Map.insert ident stackLocation offsets) stackLocation
          return $ Stack stackLocation
    other -> return other

lowerInstruction :: Instruction -> Transform PseudoRegistersPassState Instruction
lowerInstruction instruction = do
  case instruction of
    (Mov lhs rhs) -> do
      lhs' <- lowerVal lhs
      rhs' <- lowerVal rhs
      return $ Mov lhs' rhs'
    (Unary operator operand) -> do
      op' <- lowerVal operand
      return $ Unary operator op'
    (Binary operator lhs rhs) -> do
      lhs' <- lowerVal lhs
      rhs' <- lowerVal rhs
      return $ Binary operator lhs' rhs'
    (IDiv operand) -> do
      operand' <- lowerVal operand
      return $ IDiv operand'
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
