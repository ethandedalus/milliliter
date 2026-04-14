{-# LANGUAGE LambdaCase #-}

module Compiler.SemanticAnalysis.VariableResolution where

import qualified Compiler.Parser.Types as PT
import Compiler.SemanticAnalysis.Types
import Control.Lens
import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map

makeTemporary :: String -> Transform String
makeTemporary name = do
  counter <- varSeq <<%= (+ 1)
  return $ name ++ "$" ++ show counter

resolveFactor :: PT.Factor -> Transform PT.Factor
resolveFactor factor = do
  vars <- use varMap
  case factor of
    (PT.Ident ident) -> case Map.lookup ident vars of
      Nothing -> throwError (UndefinedVariable ident)
      (Just ident') -> return $ PT.Ident ident'
    (PT.Unary PT.PrefixIncrement operand)
      | isLValue operand -> PT.Unary PT.PrefixIncrement <$> resolveFactor operand
      | otherwise -> throwError InvalidLValue
    (PT.Unary PT.PostfixIncrement operand)
      | isLValue operand -> PT.Unary PT.PostfixIncrement <$> resolveFactor operand
      | otherwise -> throwError InvalidLValue
    (PT.Unary PT.PrefixDecrement operand)
      | isLValue operand -> PT.Unary PT.PrefixDecrement <$> resolveFactor operand
      | otherwise -> throwError InvalidLValue
    (PT.Unary PT.PostfixDecrement operand)
      | isLValue operand -> PT.Unary PT.PostfixDecrement <$> resolveFactor operand
      | otherwise -> throwError InvalidLValue
    (PT.Expr expr) -> PT.Expr <$> resolveExpr expr
    (PT.Unary op' f) -> PT.Unary op' <$> resolveFactor f
    other -> return other
 where
  isLValue = \case
    (PT.Ident _) -> True
    (PT.Expr (PT.Var _)) -> True
    (PT.Expr (PT.Factor _)) -> True
    _ -> False

resolveExpr :: PT.Expr -> Transform PT.Expr
resolveExpr expr = do
  vars <- use varMap
  case expr of
    (PT.Var ident) -> do
      case Map.lookup ident vars of
        Nothing -> throwError (UndefinedVariable ident)
        (Just ident') -> return $ PT.Var ident'
    (PT.Assign lhs rhs)
      | isLValue lhs -> PT.Assign <$> resolveExpr lhs <*> resolveExpr rhs
      | otherwise -> throwError InvalidLValue
    (PT.CompoundAssign op' lhs rhs)
      | isLValue lhs -> PT.CompoundAssign op' <$> resolveExpr lhs <*> resolveExpr rhs
      | otherwise -> throwError InvalidLValue
    (PT.Factor factor) -> PT.Factor <$> resolveFactor factor
    (PT.Binary op' lhs rhs) -> PT.Binary op' <$> resolveExpr lhs <*> resolveExpr rhs
 where
  isLValue = \case
    (PT.Var _) -> True
    (PT.Factor (PT.Ident _)) -> True
    _ -> False

resolveStmt :: PT.Stmt -> Transform PT.Stmt
resolveStmt = \case
  (PT.Return expr) -> PT.Return <$> resolveExpr expr
  (PT.ExprS expr) -> PT.ExprS <$> resolveExpr expr
  PT.Null -> return PT.Null

resolveDecl :: PT.Decl -> Transform PT.Decl
resolveDecl (PT.Decl name expr) = do
  vars <- use varMap
  when (Map.member name vars) $ throwError (Redefinition name)
  tmp <- makeTemporary name
  varMap %= Map.insert name tmp
  case expr of
    Nothing -> return $ PT.Decl tmp expr
    (Just expr') -> PT.Decl tmp . pure <$> resolveExpr expr'

resolveBlockItem :: PT.BlockItem -> Transform PT.BlockItem
resolveBlockItem = \case
  (PT.S stmt) -> PT.S <$> resolveStmt stmt
  (PT.D decl) -> PT.D <$> resolveDecl decl

resolveBlockItems :: [PT.BlockItem] -> Transform [PT.BlockItem]
resolveBlockItems = mapM resolveBlockItem

resolveFunc :: PT.Func -> Transform PT.Func
resolveFunc (PT.Func ident body) = PT.Func ident <$> resolveBlockItems body

resolveProgram :: PT.Program -> Transform PT.Program
resolveProgram (PT.Program f) = PT.Program <$> resolveFunc f
