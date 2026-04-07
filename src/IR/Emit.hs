module IR.Emit (lowerExpr, lowerStmt, lowerStmts, lowerFunc, lowerProgram, lowerFactor) where

import Compiler.Class (from)
import Control.Monad.State (get, put)
import IR.Types (Emitter, Func (..), Instruction (..), Program (..), Val (..))
import qualified Parser.Types as PT (Expr (..), Factor (..), Func (..), Program (..), Stmt (..))

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
      tmp <- makeTemporary "tmp"
      let dst = Var tmp
      return (instructions ++ [Unary (from op) src dst], dst)
    (PT.Expr expr) -> lowerExpr expr

lowerExpr :: PT.Expr -> Emitter ([Instruction], Val)
lowerExpr expr = do
  case expr of
    (PT.Factor factor) -> lowerFactor factor
    (PT.Binary op lhs rhs) -> do
      (instructions', _) <- lowerExpr lhs
      (instructions'', dst) <- lowerExpr rhs
      tmp <- makeTemporary "tmp"
      return (instructions' ++ instructions'', dst)

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
