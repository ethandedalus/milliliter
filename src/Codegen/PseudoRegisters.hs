module Codegen.PseudoRegisters where

import Codegen.Types
  ( Func (..),
    Instruction (..),
    Operand (..),
    Program (..),
    PseudoRegistersPassState (..),
    Transform,
  )
import Control.Monad.State (get, put)
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
    other -> return other

lowerInstructions :: [Instruction] -> Transform PseudoRegistersPassState [Instruction]
lowerInstructions = mapM lowerInstruction

lowerFunc :: Func -> Transform PseudoRegistersPassState Func
lowerFunc (Func name instructions _) = do
  instructions' <- lowerInstructions instructions
  PseudoRegistersPassState _ current <- get
  return $ Func name instructions' (-current)

lower :: Program -> Transform PseudoRegistersPassState Program
lower (Program func) = do
  func' <- lowerFunc func
  return $ Program func'
