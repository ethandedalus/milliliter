{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.IR.Transform (
  transformExpr,
  transformStmt,
  transformStmts,
  transformFunc,
  transformProgram,
) where

import qualified Compiler.AST as AST
import Compiler.Class (from)
import Compiler.IR.Types (
  BinaryOperator (..),
  Func (..),
  IRError (IRError),
  Instruction (..),
  Program (..),
  Transform,
  Val (..),
  labelSeq,
  varSeq,
 )
import Compiler.Pass
import Control.Lens (use, (%=))
import Control.Monad.Except (throwError)

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

transformLogicalAnd ::
  AST.Expr Resolved ->
  AST.Expr Resolved ->
  Transform ([Instruction], Val)
transformLogicalAnd lhs rhs = do
  falseLabel <- makeLabel "false"
  endLabel <- makeLabel "end"
  (instructions1', e1) <- transformExpr lhs
  (instructions2', e2) <- transformExpr rhs
  let jmp1 = JumpIfZero e1 falseLabel
  let jmp2 = JumpIfZero e2 falseLabel
  dst <- Var <$> makeTemporary "tmp"
  let instructions =
        instructions1'
          <> [jmp1]
          <> instructions2'
          <> [jmp2]
          <> [ Copy (Lit 1) dst
             , Jump endLabel
             , Label falseLabel
             , Copy (Lit 0) dst
             , Label endLabel
             ]
  return (instructions, dst)

transformLogicalOr ::
  AST.Expr Resolved ->
  AST.Expr Resolved ->
  Transform ([Instruction], Val)
transformLogicalOr lhs rhs = do
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
        instructions1'
          <> [jmp1]
          <> instructions2'
          <> [jmp2]
          <> [ Copy (Lit 0) dst
             , Jump endLabel
             , Label trueLabel
             , Copy (Lit 1) dst
             , Label endLabel
             ]
  return (instructions, dst)

transformConditional ::
  AST.Expr Resolved ->
  AST.Expr Resolved ->
  AST.Expr Resolved ->
  Transform ([Instruction], Val)
transformConditional l m r = do
  (condInstructions, condResult) <- transformExpr l
  (e1Instructions, e1Result) <- transformExpr m
  (e2Instructions, e2Result) <- transformExpr r
  e2Label <- makeLabel "e2"
  endLabel <- makeLabel "end"
  dst <- Var <$> makeTemporary "tmp"
  let instructions =
        condInstructions
          ++ [JumpIfZero condResult e2Label]
          ++ e1Instructions
          ++ [Copy e1Result dst, Jump endLabel, Label e2Label]
          ++ e2Instructions
          ++ [Copy e2Result dst, Label endLabel]
  return (instructions, dst)

transformCompoundAssign ::
  AST.BinaryOperator ->
  AST.Expr Resolved ->
  AST.Expr Resolved ->
  Transform ([Instruction], Val)
transformCompoundAssign op lhs rhs = do
  (instructions', lhs') <- case lhs of
    (AST.Var _ _) -> transformExpr lhs
    _ -> throwError (IRError "invalid lvalue")
  (instructions'', rhs') <- transformExpr rhs
  case op of
    AST.AddAssign -> return (instructions' <> instructions'' <> [Binary Add lhs' rhs' lhs'], lhs')
    AST.SubAssign -> return (instructions' <> instructions'' <> [Binary Sub lhs' rhs' lhs'], lhs')
    AST.MulAssign -> return (instructions' <> instructions'' <> [Binary Mul lhs' rhs' lhs'], lhs')
    AST.DivAssign -> return (instructions' <> instructions'' <> [Binary Div lhs' rhs' lhs'], lhs')
    AST.ModAssign -> return (instructions' <> instructions'' <> [Binary Mod lhs' rhs' lhs'], lhs')
    AST.AndAssign -> return (instructions' <> instructions'' <> [Binary BitAnd lhs' rhs' lhs'], lhs')
    AST.OrAssign -> return (instructions' <> instructions'' <> [Binary BitOr lhs' rhs' lhs'], lhs')
    AST.XorAssign -> return (instructions' <> instructions'' <> [Binary Xor lhs' rhs' lhs'], lhs')
    AST.ShlAssign -> return (instructions' <> instructions'' <> [Binary LeftShift lhs' rhs' lhs'], lhs')
    AST.ShrAssign -> return (instructions' <> instructions'' <> [Binary RightShift lhs' rhs' lhs'], lhs')
    _ -> throwError (IRError "invalid compound assignment operator")

transformExpr :: AST.Expr Resolved -> Transform ([Instruction], Val)
transformExpr expr = case expr of
  AST.Lit lit -> return ([], Lit lit)
  AST.VarResolved x -> return ([], Var x)
  (AST.Binary AST.And lhs rhs) -> transformLogicalAnd lhs rhs
  (AST.Binary AST.Or lhs rhs) -> transformLogicalOr lhs rhs
  (AST.Binary op lhs rhs) -> do
    (instructions', e1) <- transformExpr lhs
    (instructions'', e2) <- transformExpr rhs
    dst <- Var <$> makeTemporary "tmp"
    return (instructions' ++ instructions'' ++ [Binary (from op) e1 e2 dst], dst)
  AST.Assign lhs rhs -> do
    lhs' <- case lhs of
      (AST.VarResolved x) -> pure x
      _ -> throwError (IRError "invalid lvalue")
    (instructions', rhs') <- transformExpr rhs
    return (instructions' ++ [Copy rhs' (Var lhs')], Var lhs')
  (AST.CompoundAssign op lhs rhs) -> transformCompoundAssign op lhs rhs
  AST.Unary AST.PrefixIncrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Binary Add src (Lit 1) src, Copy src dst], dst)
  AST.Unary AST.PostfixIncrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Copy src dst, Binary Add src (Lit 1) src], dst)
  AST.Unary AST.PrefixDecrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Binary Sub src (Lit 1) src, Copy src dst], dst)
  AST.Unary AST.PostfixDecrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Copy src dst, Binary Sub src (Lit 1) src], dst)
  AST.Unary op inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Unary (from op) src dst], dst)
  AST.ConditionalE l m r -> transformConditional l m r
  _ -> error "ICE: (IR) unreachable"

transformIf :: AST.Expr Resolved -> AST.Stmt Resolved -> Maybe (AST.Stmt Resolved) -> Transform [Instruction]
transformIf a b (Just c) = do
  (condInstructions, condResult) <- transformExpr a
  s1Instructions <- transformStmt b
  s2Instructions <- transformStmt c
  falseLabel <- makeLabel "false"
  endLabel <- makeLabel "end"
  return $
    condInstructions
      ++ [JumpIfZero condResult falseLabel]
      ++ s1Instructions
      ++ [Jump endLabel, Label falseLabel]
      ++ s2Instructions
      ++ [Label endLabel]
transformIf a b Nothing = do
  (condInstructions, condResult) <- transformExpr a
  stmtInstructions <- transformStmt b
  endLabel <- makeLabel "end"
  return $
    condInstructions
      ++ [JumpIfZero condResult endLabel]
      ++ stmtInstructions
      ++ [Label endLabel]

transformStmt :: AST.Stmt Resolved -> Transform [Instruction]
transformStmt stmt = case stmt of
  (AST.Return expr) -> do
    (instructions, inner) <- transformExpr expr
    return $ instructions ++ [Return inner]
  (AST.ExprS expr) -> fst <$> transformExpr expr
  AST.If a b c -> transformIf a b c
  AST.Null -> return []
  AST.Compound (AST.Block _ items) -> concat <$> mapM transformBlockItem items
  AST.Label _ label s -> (Label label :) <$> transformStmt s
  AST.Goto _ label -> return [Jump label]

transformStmts :: [AST.Stmt Resolved] -> Transform [Instruction]
transformStmts stmts = concat <$> mapM transformStmt stmts

transformBlockItem :: AST.BlockItem Resolved -> Transform [Instruction]
transformBlockItem = \case
  AST.S stmt -> transformStmt stmt
  (AST.D (AST.Decl _ Nothing)) -> return []
  (AST.D (AST.Decl ident (Just expr))) -> do
    (instructions', expr') <- transformExpr expr
    return $ instructions' ++ [Copy expr' (Var ident)]

transformFunc :: AST.Func Resolved -> Transform Func
transformFunc (AST.Func name (AST.Block _ blockItems)) =
  Func name . concat
    <$> mapM transformBlockItem (blockItems ++ [return0])
 where
  return0 = AST.S (AST.Return (AST.Lit 0))

transformProgram :: AST.Program Resolved -> Transform Program
transformProgram (AST.Program func) = Program <$> transformFunc func
