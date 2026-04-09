module Compiler.IR.Lower (lowerExpr, lowerStmt, lowerStmts, lowerFunc, lowerProgram, lowerFactor) where

import Compiler.Class (from)
import Compiler.IR.Types (Emitter, Func (..), Instruction (..), Program (..), Val (..))
import qualified Compiler.Parser.Types as PT (Expr (..), Factor (..), Func (..), Program (..), Stmt (..))
import Control.Monad.State (get, put)

makeTemporary :: String -> Emitter String
makeTemporary tmp = do
  counter <- get
  put (counter + 1)
  return $ tmp ++ "." ++ show counter

lowerFactor :: PT.Factor -> Emitter ([Instruction], Val)
lowerFactor factor = do
  case factor of
    (PT.Lit lit) -> return ([], Lit lit)
    (PT.Unary op inner) -> do
      (instructions, src) <- lowerFactor inner
      dst <- Var <$> makeTemporary "tmp"
      return (instructions ++ [Unary (from op) src dst], dst)
    (PT.Expr expr) -> lowerExpr expr

lowerExpr :: PT.Expr -> Emitter ([Instruction], Val)
lowerExpr expr = do
  case expr of
    (PT.Factor factor) -> lowerFactor factor
    (PT.Binary op lhs rhs) -> do
      (instructions', e1) <- lowerExpr lhs
      (instructions'', e2) <- lowerExpr rhs
      dst <- Var <$> makeTemporary "tmp"
      return (instructions' ++ instructions'' ++ [Binary (from op) e1 e2 dst], dst)

lowerStmt :: PT.Stmt -> Emitter [Instruction]
lowerStmt stmt = do
  case stmt of
    (PT.Return expr) -> do
      (instructions, inner) <- lowerExpr expr
      return $ instructions ++ [Return inner]

lowerStmts :: [PT.Stmt] -> Emitter [Instruction]
lowerStmts stmts = concat <$> mapM lowerStmt stmts

lowerFunc :: PT.Func -> Emitter Func
lowerFunc (PT.Func name stmt) = lowerStmt stmt >>= \instructions -> return $ Func name instructions

lowerProgram :: PT.Program -> Emitter Program
lowerProgram (PT.Program func) = lowerFunc func >>= \f -> return $ Program f
