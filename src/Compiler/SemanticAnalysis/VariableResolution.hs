{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.SemanticAnalysis.VariableResolution where

import Compiler.AST
import Compiler.Pass
import Compiler.SemanticAnalysis.Types
import Control.Lens
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

withScope :: ScopeID -> Transform a -> Transform a
withScope sid = local (scopeStack %~ (sid :))

makeTemporary :: String -> Transform String
makeTemporary n = do
  counter <- varSeq <<+= 1
  prefix <- maybe "" (++ "@") <$> view func
  return $ prefix ++ n ++ "$" ++ show counter

resolveVar :: String -> Transform (Expr Resolved)
resolveVar ident = do
  stack <- view scopeStack
  vars <- use varMap
  let result = asum $ (\sid -> Map.lookup sid vars >>= Map.lookup ident) <$> stack
  case result of
    Nothing -> throwError (UndefinedVariable ident)
    Just ident' -> return $ Var 0 ident'

resolveExpr :: Expr Parsed -> Transform (Expr Resolved)
resolveExpr expr = do
  case expr of
    (VarParsed ident) -> resolveVar ident
    (Assign lhs rhs)
      | isLValue lhs -> Assign <$> resolveExpr lhs <*> resolveExpr rhs
      | otherwise -> throwError InvalidLValue
    (CompoundAssign op' lhs rhs)
      | isLValue lhs -> CompoundAssign op' <$> resolveExpr lhs <*> resolveExpr rhs
      | otherwise -> throwError InvalidLValue
    (Binary op' lhs rhs) -> Binary op' <$> resolveExpr lhs <*> resolveExpr rhs
    (Unary PrefixIncrement operand)
      | isLValue operand -> Unary PrefixIncrement <$> resolveExpr operand
      | otherwise -> throwError InvalidLValue
    (Unary PrefixDecrement operand)
      | isLValue operand -> Unary PrefixDecrement <$> resolveExpr operand
      | otherwise -> throwError InvalidLValue
    (Unary PostfixIncrement operand)
      | isLValue operand -> Unary PostfixIncrement <$> resolveExpr operand
      | otherwise -> throwError InvalidLValue
    (Unary PostfixDecrement operand)
      | isLValue operand -> Unary PostfixDecrement <$> resolveExpr operand
      | otherwise -> throwError InvalidLValue
    Unary op' expr' -> Unary op' <$> resolveExpr expr'
    ConditionalE l m r -> ConditionalE <$> resolveExpr l <*> resolveExpr m <*> resolveExpr r
    (Lit lit) -> return (Lit lit)
    _ -> error "ICE: unreachable"
 where
  isLValue = \case
    (VarParsed _) -> True
    _ -> False

resolveStmt :: Stmt Parsed -> Transform (Stmt Resolved)
resolveStmt = \case
  Return expr -> Return <$> resolveExpr expr
  ExprS expr -> ExprS <$> resolveExpr expr
  If a b c -> If <$> resolveExpr a <*> resolveStmt b <*> traverse resolveStmt c
  Null -> return Null
  Compound b -> Compound <$> resolveBlock b
  LabelParsed ident stmt -> do
    enclosingFunc <- view func >>= maybe (throwError (SemanticAnalysisError "label outside function")) return
    isDuplicate <- uses funcLabels (Set.member ident . fromMaybe Set.empty . Map.lookup enclosingFunc)
    when isDuplicate $ throwError (DuplicateLabel ident enclosingFunc)

    stmt' <- resolveStmt stmt
    funcLabels . at enclosingFunc . non Set.empty %= Set.insert ident
    return $ Label enclosingFunc ident stmt'
  GotoParsed label -> do
    enclosingFunc <- view func >>= maybe (throwError (SemanticAnalysisError "label outside function")) return
    return $ Goto enclosingFunc label
  _ -> error "ICE: unreachable"

resolveDecl :: Decl Parsed -> Transform (Decl Resolved)
resolveDecl (Decl d expr) = do
  sid <- view (scopeStack . to head)
  currentScopeMap <- use (varMap . at sid) <&> fromMaybe Map.empty
  when (Map.member d currentScopeMap) $ throwError (Redefinition d)
  tmp <- makeTemporary d
  varMap . at sid . non Map.empty . at d .= Just tmp
  case expr of
    Nothing -> return $ Decl tmp Nothing
    (Just expr') -> Decl tmp . pure <$> resolveExpr expr'

resolveBlockItem :: BlockItem Parsed -> Transform (BlockItem Resolved)
resolveBlockItem = \case
  (S stmt) -> S <$> resolveStmt stmt
  (D decl) -> D <$> resolveDecl decl

resolveBlockItems :: [BlockItem Parsed] -> Transform [BlockItem Resolved]
resolveBlockItems = mapM resolveBlockItem

resolveBlock :: Block Parsed -> Transform (Block Resolved)
resolveBlock (BlockParsed items) = scopeSeq <<+= 1 >>= \sid' -> withScope sid' $ Block sid' <$> resolveBlockItems items
resolveBlock _ = error "ICE: unreachable"

resolveFunc :: Func Parsed -> Transform (Func Resolved)
resolveFunc (Func ident funcBody) = local (func ?~ ident) $ Func ident <$> resolveBlock funcBody

resolveProgram :: Program Parsed -> Transform (Program Resolved)
resolveProgram (Program f) = scopeSeq <<+= 1 >>= \sid' -> withScope sid' $ Program <$> resolveFunc f
