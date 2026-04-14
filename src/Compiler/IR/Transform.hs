{-# LANGUAGE LambdaCase #-}

module Compiler.IR.Transform (
  transformExpr,
  transformStmt,
  transformStmts,
  transformFunc,
  transformProgram,
  transformFactor,
) where

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
import qualified Compiler.Parser.Types as PT (
  BinaryOperator (..),
  BlockItem (..),
  Decl (..),
  Expr (..),
  Factor (..),
  Func (..),
  Program (..),
  Stmt (..),
  UnaryOperator (..),
 )
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

transformFactor :: PT.Factor -> Transform ([Instruction], Val)
transformFactor factor = case factor of
  (PT.Lit lit) -> return ([], Lit lit)
  (PT.Unary PT.PrefixIncrement inner) -> do
    (instructions, src) <- transformFactor inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Binary Add src (Lit 1) src, Copy src dst], dst)
  (PT.Unary PT.PostfixIncrement inner) -> do
    (instructions, src) <- transformFactor inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Copy src dst, Binary Add src (Lit 1) src], dst)
  (PT.Unary PT.PrefixDecrement inner) -> do
    (instructions, src) <- transformFactor inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Binary Sub src (Lit 1) src, Copy src dst], dst)
  (PT.Unary PT.PostfixDecrement inner) -> do
    (instructions, src) <- transformFactor inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Copy src dst, Binary Sub src (Lit 1) src], dst)
  (PT.Unary op inner) -> do
    (instructions, src) <- transformFactor inner
    dst <- Var <$> makeTemporary "tmp"
    return (instructions ++ [Unary (from op) src dst], dst)
  (PT.Expr expr) -> transformExpr expr
  (PT.Ident ident) -> return ([], Var ident)

transformExpr :: PT.Expr -> Transform ([Instruction], Val)
transformExpr expr = case expr of
  (PT.Var x) -> return ([], Var x)
  (PT.Factor factor) -> transformFactor factor
  (PT.Assign lhs rhs) -> do
    lhs' <- case lhs of
      (PT.Var name) -> pure name
      (PT.Factor (PT.Ident name)) -> pure name
      _ -> throwError (IRError "invalid lvalue")
    (instructions', rhs') <- transformExpr rhs
    return (instructions' ++ [Copy rhs' (Var lhs')], Var lhs')
  (PT.CompoundAssign op lhs rhs) -> do
    (instructions', lhs') <- case lhs of
      (PT.Var _) -> transformExpr lhs
      (PT.Factor (PT.Ident _)) -> transformExpr lhs
      _ -> throwError (IRError "invalid lvalue")
    (instructions'', rhs') <- transformExpr rhs
    case op of
      PT.AddAssign -> return (instructions' ++ instructions'' ++ [Binary Add lhs' rhs' lhs'], lhs')
      PT.SubAssign -> return (instructions' ++ instructions'' ++ [Binary Sub lhs' rhs' lhs'], lhs')
      PT.MulAssign -> return (instructions' ++ instructions'' ++ [Binary Mul lhs' rhs' lhs'], lhs')
      PT.DivAssign -> return (instructions' ++ instructions'' ++ [Binary Div lhs' rhs' lhs'], lhs')
      PT.ModAssign -> return (instructions' ++ instructions'' ++ [Binary Mod lhs' rhs' lhs'], lhs')
      PT.AndAssign -> return (instructions' ++ instructions'' ++ [Binary BitAnd lhs' rhs' lhs'], lhs')
      PT.OrAssign -> return (instructions' ++ instructions'' ++ [Binary BitOr lhs' rhs' lhs'], lhs')
      PT.XorAssign -> return (instructions' ++ instructions'' ++ [Binary Xor lhs' rhs' lhs'], lhs')
      PT.ShlAssign -> return (instructions' ++ instructions'' ++ [Binary LeftShift lhs' rhs' lhs'], lhs')
      PT.ShrAssign -> return (instructions' ++ instructions'' ++ [Binary RightShift lhs' rhs' lhs'], lhs')
      _ -> throwError (IRError "invalid compound assignment operator")
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
  (PT.ExprS expr) -> fst <$> transformExpr expr
  PT.Null -> return []

transformStmts :: [PT.Stmt] -> Transform [Instruction]
transformStmts stmts = concat <$> mapM transformStmt stmts

transformBlockItem :: PT.BlockItem -> Transform [Instruction]
transformBlockItem = \case
  PT.S stmt -> transformStmt stmt
  (PT.D (PT.Decl _ Nothing)) -> return []
  (PT.D (PT.Decl ident (Just expr))) -> do
    (instructions', expr') <- transformExpr expr
    return $ instructions' ++ [Copy expr' (Var ident)]

transformFunc :: PT.Func -> Transform Func
transformFunc (PT.Func name blockItems) = Func name . concat <$> mapM transformBlockItem (blockItems ++ [PT.S (PT.Return (PT.Factor $ PT.Lit 0))])

transformProgram :: PT.Program -> Transform Program
transformProgram (PT.Program func) = Program <$> transformFunc func
