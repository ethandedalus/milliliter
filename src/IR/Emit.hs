module IR.Emit (lowerExpr, lowerStmt, lowerStmts, lowerFunc, lowerProgram) where

import Compiler.Class (from)
import Control.Monad.State (get, put)
import IR.Types (Emitter, Func (..), Instruction (..), Program (..), Val (..))
import qualified Parser.Types as PT (Expr (..), Func (..), Program (..), Stmt (..))

makeTemporary :: String -> Emitter String
makeTemporary tmp = do
  counter <- get
  put (counter + 1)
  return $ tmp ++ "." ++ show counter

lowerExpr :: PT.Expr -> Emitter ([Instruction], Val)
lowerExpr expr = do
  case expr of
    (PT.Lit lit) -> return ([], Lit lit)
    (PT.Unary op inner) -> do
      (instructions, src) <- lowerExpr inner
      tmp <- makeTemporary "tmp"
      let dst = Var tmp
      return (instructions ++ [Unary (from op) src dst], dst)

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
