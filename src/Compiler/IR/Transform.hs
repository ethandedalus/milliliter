module Compiler.IR.Transform (
  transformExpr,
  transformStmt,
  transformStmts,
  transformFunc,
  transformProgram,
  transformFactor,
) where

import Compiler.Class (from)
import Compiler.IR.Types (Func (..), Instruction (..), Program (..), Transform, Val (..), labelSeq, varSeq)
import qualified Compiler.Parser.Types as PT (
  BinaryOperator (..),
  Expr (..),
  Factor (..),
  Func (..),
  Program (..),
  Stmt (..),
 )
import Control.Lens (use, (%=))

makeTemporary :: String -> Transform String
makeTemporary tmp = do
  counter <- use varSeq
  varSeq %= (+ 1)
  return $ tmp ++ "." ++ show counter

makeLabel :: String -> Transform String
makeLabel tmp = do
  counter <- use labelSeq
  labelSeq %= (+ 1)
  return $ '_' : tmp ++ show counter

transformFactor :: PT.Factor -> Transform ([Instruction], Val)
transformFactor factor = case factor of
  (PT.Lit lit) -> return ([], Lit lit)
  (PT.Unary op inner) -> do
    (instructions, src) <- transformFactor inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Unary (from op) src dst], dst)
  (PT.Expr expr) -> transformExpr expr

transformExpr :: PT.Expr -> Transform ([Instruction], Val)
transformExpr expr = case expr of
  (PT.Factor factor) -> transformFactor factor
  (PT.Binary PT.And lhs rhs) -> do
    falseLabel <- makeLabel "false"
    endLabel <- makeLabel "end"
    (instructions1', e1) <- transformExpr lhs
    (instructions2', e2) <- transformExpr rhs
    let jmp1 = JumpIfZero e1 falseLabel
    let jmp2 = JumpIfZero e2 falseLabel
    dst <- Var <$> makeTemporary "tmp"
    let instructions =
          concat
            [ instructions1'
            , [jmp1]
            , instructions2'
            , [jmp2]
            ,
              [ Copy (Lit 1) dst
              , Jump endLabel
              , Label falseLabel
              , Copy (Lit 0) dst
              , Label endLabel
              ]
            ]
    return (instructions, dst)
  (PT.Binary PT.Or lhs rhs) -> do
    trueLabel <- makeLabel "true"
    endLabel <- makeLabel "end"
    -- if either e1 or e2 are true, jump to the true label, set dst to 1, then jump to end
    -- if not, then set result to true and jump to end
    (instructions1', e1) <- transformExpr lhs
    (instructions2', e2) <- transformExpr rhs
    let jmp1 = JumpIfNonZero e1 trueLabel
    let jmp2 = JumpIfNonZero e2 trueLabel
    dst <- Var <$> makeTemporary "tmp"
    let instructions =
          concat
            [ instructions1'
            , [jmp1]
            , instructions2'
            , [jmp2]
            ,
              [ Copy (Lit 0) dst
              , Jump endLabel
              , Label trueLabel
              , Copy (Lit 1) dst
              , Label endLabel
              ]
            ]
    return (instructions, dst)
  (PT.Binary op lhs rhs) -> do
    (instructions', e1) <- transformExpr lhs
    (instructions'', e2) <- transformExpr rhs
    dst <- Var <$> makeTemporary "tmp"
    return (instructions' ++ instructions'' ++ [Binary (from op) e1 e2 dst], dst)

transformStmt :: PT.Stmt -> Transform [Instruction]
transformStmt stmt = case stmt of
  (PT.Return expr) -> do
    (instructions, inner) <- transformExpr expr
    return $ instructions ++ [Return inner]

transformStmts :: [PT.Stmt] -> Transform [Instruction]
transformStmts stmts = concat <$> mapM transformStmt stmts

transformFunc :: PT.Func -> Transform Func
transformFunc (PT.Func name stmt) = transformStmt stmt >>= \instructions -> return $ Func name instructions

transformProgram :: PT.Program -> Transform Program
transformProgram (PT.Program func) = transformFunc func >>= \f -> return $ Program f
