{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | This module handles lowering from a resolved AST to a three-address-code (TAC) representation
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
import Compiler.Pass (Pass (Resolved))
import Control.Lens ((<<+=))
import Control.Monad.Except (throwError)
import Data.Bifunctor (first)

-- | makeTemporary creates a new, globally unique temporary variable in the format @tmp.x@, where x is a globally incremented counter
makeTemporary :: Transform String
makeTemporary = varSeq <<+= 1 >>= \c -> return ("tmp." ++ show c)

-- | makeLabel creates a new, globally unique label in the format @_label@, where label is a syntatically valid identifier
makeLabel :: String -> Transform String
makeLabel tmp = labelSeq <<+= 1 >>= \c -> return ('_' : tmp ++ show c)

{- | Generates TAC for the @&&@ operator with short-circuit evaluation.

If the LHS evaluates to zero, the RHS is not evaluated and the result is @0@.
Otherwise, if the RHS evaluates to zero, the result is @0@. If both are
non-zero, the result is @1@.
-}
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
  dst <- Var <$> makeTemporary
  let instructions =
        instructions1'
          ++ [jmp1]
          ++ instructions2'
          ++ [jmp2]
          ++ [ Copy (Lit 1) dst
             , Jump endLabel
             , Label falseLabel
             , Copy (Lit 0) dst
             , Label endLabel
             ]
  return (instructions, dst)

{- | Generates TAC for the @||@ operator with short-circuit evaluation.

If the LHS evaluates to one, the RHS is not evaluated and the result is @1@.
Otherwise, if the RHS evaluates to 1, the result is @1@. If both are
zero, the result is @0@.
-}
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
  dst <- Var <$> makeTemporary
  let instructions =
        instructions1'
          ++ [jmp1]
          ++ instructions2'
          ++ [jmp2]
          ++ [ Copy (Lit 0) dst
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
  dst <- Var <$> makeTemporary
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
    AST.AddAssign -> return (instructions' ++ instructions'' ++ [Binary Add lhs' rhs' lhs'], lhs')
    AST.SubAssign -> return (instructions' ++ instructions'' ++ [Binary Sub lhs' rhs' lhs'], lhs')
    AST.MulAssign -> return (instructions' ++ instructions'' ++ [Binary Mul lhs' rhs' lhs'], lhs')
    AST.DivAssign -> return (instructions' ++ instructions'' ++ [Binary Div lhs' rhs' lhs'], lhs')
    AST.ModAssign -> return (instructions' ++ instructions'' ++ [Binary Mod lhs' rhs' lhs'], lhs')
    AST.AndAssign -> return (instructions' ++ instructions'' ++ [Binary BitAnd lhs' rhs' lhs'], lhs')
    AST.OrAssign -> return (instructions' ++ instructions'' ++ [Binary BitOr lhs' rhs' lhs'], lhs')
    AST.XorAssign -> return (instructions' ++ instructions'' ++ [Binary Xor lhs' rhs' lhs'], lhs')
    AST.ShlAssign -> return (instructions' ++ instructions'' ++ [Binary LeftShift lhs' rhs' lhs'], lhs')
    AST.ShrAssign -> return (instructions' ++ instructions'' ++ [Binary RightShift lhs' rhs' lhs'], lhs')
    _ -> throwError (IRError "invalid compound assignment operator")

{- | Lowers a resolved expression to a 2-tuple with a flat list of TAC instructions and the result of that expression

Panics on any expression variant that should have been eliminated by prior passes.
-}
transformExpr :: AST.Expr Resolved -> Transform ([Instruction], Val)
transformExpr expr = case expr of
  AST.Lit lit -> return ([], Lit lit)
  AST.VarR x -> return ([], Var x)
  (AST.Binary AST.And lhs rhs) -> transformLogicalAnd lhs rhs
  (AST.Binary AST.Or lhs rhs) -> transformLogicalOr lhs rhs
  (AST.Binary op lhs rhs) -> do
    (instructions', e1) <- transformExpr lhs
    (instructions'', e2) <- transformExpr rhs
    dst <- Var <$> makeTemporary
    return (instructions' ++ instructions'' ++ [Binary (from op) e1 e2 dst], dst)
  AST.Assign lhs rhs -> do
    lhs' <- case lhs of
      (AST.VarR x) -> pure x
      _ -> error "ICE: all lhs should've been semantically validated as valid lvalues"
    (instructions', rhs') <- transformExpr rhs
    return (instructions' ++ [Copy rhs' (Var lhs')], Var lhs')
  (AST.CompoundAssign op lhs rhs) -> transformCompoundAssign op lhs rhs
  AST.Unary AST.PrefixIncrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary
    return (instructions ++ [Binary Add src (Lit 1) src, Copy src dst], dst)
  AST.Unary AST.PostfixIncrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary
    return (instructions ++ [Copy src dst, Binary Add src (Lit 1) src], dst)
  AST.Unary AST.PrefixDecrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary
    return (instructions ++ [Binary Sub src (Lit 1) src, Copy src dst], dst)
  AST.Unary AST.PostfixDecrement inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary
    return (instructions ++ [Copy src dst, Binary Sub src (Lit 1) src], dst)
  AST.Unary op inner -> do
    (instructions, src) <- transformExpr inner
    dst <- Var <$> makeTemporary
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

transformWhile :: AST.LoopID -> AST.Expr Resolved -> AST.Stmt Resolved -> Transform [Instruction]
transformWhile lid expr stmt' = do
  (condInstructions, condResult) <- transformExpr expr
  bodyInstructions <- transformStmt stmt'
  return $
    [Label ("continue" ++ show lid)]
      ++ condInstructions
      ++ [JumpIfZero condResult ("break_loop" ++ show lid)]
      ++ bodyInstructions
      ++ [Jump ("continue" ++ show lid), Label ("break_loop" ++ show lid)]

transformDoWhile :: AST.LoopID -> AST.Stmt Resolved -> AST.Expr Resolved -> Transform [Instruction]
transformDoWhile lid stmt' expr = do
  bodyInstructions <- transformStmt stmt'
  (condInstructions, condResult) <- transformExpr expr
  startLabel <- makeLabel "start"
  return $
    [Label startLabel]
      ++ bodyInstructions
      ++ [Label ("continue" ++ show lid)]
      ++ condInstructions
      ++ [JumpIfNonZero condResult startLabel, Label ("break_loop" ++ show lid)]

transformForInit :: AST.ForInit Resolved -> Transform [Instruction]
transformForInit (AST.InitDecl (AST.Decl ident (Just expr))) = transformExpr expr >>= \(i, v) -> return (i ++ [Copy v (Var ident)])
transformForInit (AST.InitDecl (AST.Decl _ Nothing)) = return []
transformForInit (AST.InitExpr expr) = fst <$> transformExpr expr
transformForInit AST.Empty = return []

transformFor ::
  AST.LoopID ->
  AST.ForInit Resolved ->
  Maybe (AST.Expr Resolved) ->
  Maybe (AST.Expr Resolved) ->
  AST.Stmt Resolved ->
  Transform [Instruction]
transformFor lid forInit (Just condition) (Just post) body = do
  initInstructions <- transformForInit forInit
  (condInstructions, condResult) <- transformExpr condition
  postInstructions <- fst <$> transformExpr post
  bodyInstructions <- transformStmt body
  startLabel <- makeLabel "start"
  let instructions =
        initInstructions
          ++ [Label startLabel]
          ++ condInstructions
          ++ [JumpIfZero condResult ("break_loop" ++ show lid)]
          ++ bodyInstructions
          ++ [Label ("continue" ++ show lid)]
          ++ postInstructions
          ++ [Jump startLabel, Label ("break_loop" ++ show lid)]
  return instructions
transformFor lid forInit (Just condition) Nothing body = do
  initInstructions <- transformForInit forInit
  (condInstructions, condResult) <- transformExpr condition
  bodyInstructions <- transformStmt body
  startLabel <- makeLabel "start"
  let instructions =
        initInstructions
          ++ [Label startLabel]
          ++ condInstructions
          ++ [JumpIfZero condResult ("break_loop" ++ show lid)]
          ++ bodyInstructions
          ++ [Label ("continue" ++ show lid)]
          ++ [Jump startLabel, Label ("break_loop" ++ show lid)]
  return instructions
transformFor lid forInit Nothing (Just post) body = do
  initInstructions <- transformForInit forInit
  postInstructions <- fst <$> transformExpr post
  bodyInstructions <- transformStmt body
  startLabel <- makeLabel "start"
  let instructions =
        initInstructions
          ++ [Label startLabel]
          ++ bodyInstructions
          ++ [Label ("continue" ++ show lid)]
          ++ postInstructions
          ++ [Jump startLabel, Label ("break_loop" ++ show lid)]
  return instructions
transformFor lid forInit Nothing Nothing body = do
  initInstructions <- transformForInit forInit
  bodyInstructions <- transformStmt body
  startLabel <- makeLabel "start"
  let instructions =
        initInstructions
          ++ [Label startLabel]
          ++ bodyInstructions
          ++ [Label ("continue" ++ show lid)]
          ++ [Jump startLabel, Label ("break_loop" ++ show lid)]
  return instructions

transformSwitch :: AST.SwitchData -> AST.Expr Resolved -> AST.Stmt Resolved -> Transform [Instruction]
transformSwitch (AST.SwitchData i cs hasDefault) e b = do
  (exprInstructions, _) <- transformExpr e
  x <- mapM (\v -> (v,) <$> transformExpr (AST.Binary AST.NotEqual e (AST.Lit v))) cs
  let (allInstructions, allJumps) = first concat <$> unzip $ (\(v', (is, iv)) -> (is, JumpIfZero iv ("case" ++ show i ++ "_" ++ show v'))) <$> x
  let autoBreakIfNoCases = ([Jump ("break_switch" ++ show i) | null cs])
  let jumpToDefault = ([Jump ("default" ++ show i) | hasDefault])
  let jumpToEnd = [Jump ("break_switch" ++ show i)]
  let endLabel = "break_switch" ++ show i
  bodyInstrs <- transformStmt b
  return $
    exprInstructions
      ++ autoBreakIfNoCases
      ++ allInstructions
      ++ allJumps
      ++ jumpToDefault
      ++ jumpToEnd
      ++ bodyInstrs
      ++ [Label endLabel]

{- | transformStmt lowers a resolved statement to a flat list of TAC instructions.

Panics on any statement variant that should have been eliminated by prior passes.
-}
transformStmt :: AST.Stmt Resolved -> Transform [Instruction]
transformStmt stmt = case stmt of
  AST.Return expr -> transformExpr expr >>= \(is, inner) -> return (is ++ [Return inner])
  AST.ExprS expr -> fst <$> transformExpr expr
  AST.If a b c -> transformIf a b c
  AST.Null -> return []
  AST.Compound (AST.Block _ items) -> concat <$> mapM transformBlockItem items
  AST.Label _ label s -> (Label label :) <$> transformStmt s
  AST.Goto _ label -> return [Jump label]
  AST.BreakR (AST.LoopID lid) -> return [Jump ("break_loop" ++ show lid)]
  AST.BreakR (AST.SwitchID sid) -> return [Jump ("break_switch" ++ show sid)]
  AST.ContinueR lid -> return [Jump ("continue" ++ show lid)]
  AST.DoWhileR lid stmt' expr -> transformDoWhile lid stmt' expr
  AST.WhileR lid expr stmt' -> transformWhile lid expr stmt'
  AST.ForR lid forInit condition post body -> transformFor lid forInit condition post body
  AST.CaseR i (AST.Lit v) -> return [Label ("case" ++ show i ++ "_" ++ show v)]
  AST.CaseR _ _ -> error "ICE: case expressions should be reduced to literal integers before IR lowering"
  AST.DefaultR i -> return [Label ("default" ++ show i)]
  AST.SwitchR d e b -> transformSwitch d e b
  _ -> error "ICE: unreachable"

{- | transformStmts lowers multiple resolved statements to a flat list of TAC instructions.

This is just a convenience function.
-}
transformStmts :: [AST.Stmt Resolved] -> Transform [Instruction]
transformStmts stmts = concat <$> mapM transformStmt stmts

-- | transformBlockItem lowers a block item to a flat list of TAC instructions
transformBlockItem :: AST.BlockItem Resolved -> Transform [Instruction]
transformBlockItem = \case
  AST.S stmt -> transformStmt stmt
  (AST.D (AST.Decl _ Nothing)) -> return []
  (AST.D (AST.Decl ident (Just expr))) -> do
    (instructions', expr') <- transformExpr expr
    return $ instructions' ++ [Copy expr' (Var ident)]

-- | transformFunc lowers a function to a TAC function
transformFunc :: AST.Func Resolved -> Transform Func
transformFunc (AST.Func name (AST.Block _ blockItems)) =
  Func name . concat
    <$> mapM transformBlockItem (blockItems ++ [return0])
 where
  return0 = AST.S (AST.Return (AST.Lit 0))

-- | transformProgram lowers a full program to a TAC program
transformProgram :: AST.Program Resolved -> Transform Program
transformProgram (AST.Program func) = Program <$> transformFunc func
