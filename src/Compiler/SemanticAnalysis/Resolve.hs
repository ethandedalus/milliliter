{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.SemanticAnalysis.Resolve where

import Compiler.AST
import Compiler.Pass
import Compiler.SemanticAnalysis.LabelValidation
import Compiler.SemanticAnalysis.PopulateSwitchCases
import Compiler.SemanticAnalysis.Types
import Control.Lens hiding (Empty)
import Control.Monad (when, (>=>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set

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
    Just ident' -> return $ VarR ident'

resolveForInit :: ForInit Parsed -> Transform (ForInit Resolved)
resolveForInit = \case
  InitExpr e -> InitExpr <$> resolveExpr e
  InitDecl d -> InitDecl <$> resolveDecl d
  Empty -> return Empty

resolveExpr :: Expr Parsed -> Transform (Expr Resolved)
resolveExpr expr = do
  case expr of
    (VarP ident) -> resolveVar ident
    (Assign lhs rhs)
      | isLValue lhs -> Assign <$> resolveExpr lhs <*> resolveExpr rhs
      | otherwise -> throwError (InvalidLValue lhs)
    (CompoundAssign op' lhs rhs)
      | isLValue lhs -> CompoundAssign op' <$> resolveExpr lhs <*> resolveExpr rhs
      | otherwise -> throwError (InvalidLValue lhs)
    (Binary op' lhs rhs) -> Binary op' <$> resolveExpr lhs <*> resolveExpr rhs
    (Unary PrefixIncrement operand)
      | isLValue operand -> Unary PrefixIncrement <$> resolveExpr operand
      | otherwise -> throwError (InvalidLValue operand)
    (Unary PrefixDecrement operand)
      | isLValue operand -> Unary PrefixDecrement <$> resolveExpr operand
      | otherwise -> throwError (InvalidLValue operand)
    (Unary PostfixIncrement operand)
      | isLValue operand -> Unary PostfixIncrement <$> resolveExpr operand
      | otherwise -> throwError (InvalidLValue operand)
    (Unary PostfixDecrement operand)
      | isLValue operand -> Unary PostfixDecrement <$> resolveExpr operand
      | otherwise -> throwError (InvalidLValue operand)
    Unary op' expr' -> Unary op' <$> resolveExpr expr'
    ConditionalE l m r -> ConditionalE <$> resolveExpr l <*> resolveExpr m <*> resolveExpr r
    (Lit lit) -> return (Lit lit)
    _ -> error "ICE: unreachable"
 where
  isLValue = \case
    (VarP _) -> True
    _ -> False

resolveStmt :: Stmt Parsed -> Transform (Stmt Resolved)
resolveStmt = \case
  Return expr -> Return <$> resolveExpr expr
  ExprS expr -> ExprS <$> resolveExpr expr
  If a b c -> If <$> resolveExpr a <*> resolveStmt b <*> traverse resolveStmt c
  Null -> return Null
  Compound b -> Compound <$> resolveBlock b
  LabelP ident stmt -> do
    enclosingFunc <- view func >>= maybe (throwError (SemanticAnalysisError "label outside function")) return
    isDuplicate <- uses funcLabels (Set.member ident . fromMaybe Set.empty . Map.lookup enclosingFunc)
    when isDuplicate $ throwError (DuplicateLabel ident enclosingFunc)
    funcLabels . at enclosingFunc . non Set.empty %= Set.insert ident
    stmt' <- resolveStmt stmt
    return $ Label enclosingFunc ident stmt'
  GotoP label -> do
    enclosingFunc <- view func >>= maybe (throwError (SemanticAnalysisError "label outside function")) return
    return $ Goto enclosingFunc label
  BreakP -> do
    stack <- view controlStack
    controlID <- maybe (throwError $ IllegalControlStatement "break/continue outside of control statement") return (listToMaybe stack)
    return $ BreakR controlID
  ContinueP -> do
    stack <- view controlStack
    loopID <-
      maybe (throwError $ IllegalControlStatement "continue statement outside of loop") return $
        listToMaybe [sid | LoopID sid <- stack]
    return (ContinueR loopID)
  WhileP x y ->
    scopeSeq <<+= 1
      >>= \sid -> controlSeq <<+= 1 >>= \cx -> withScope sid $ withContext (LoopID cx) $ WhileR cx <$> resolveExpr x <*> resolveStmt y
  DoWhileP x y ->
    scopeSeq <<+= 1
      >>= \sid -> controlSeq <<+= 1 >>= \cx -> withScope sid $ withContext (LoopID cx) $ DoWhileR cx <$> resolveStmt x <*> resolveExpr y
  ForP a b c d ->
    scopeSeq <<+= 1 >>= \sid ->
      controlSeq <<+= 1 >>= \cx ->
        withScope sid $
          withContext (LoopID cx) $
            ForR cx <$> resolveForInit a <*> traverse resolveExpr b <*> traverse resolveExpr c <*> resolveStmt d
  SwitchP x y -> controlSeq <<+= 1 >>= \cx -> withContext (SwitchID cx) $ SwitchR (SwitchData{switchID = cx, caseValues = [], hasDefault = False}) <$> resolveExpr x <*> resolveStmt y
  CaseP x -> do
    stack <- view controlStack
    controlID <-
      maybe (throwError $ IllegalControlStatement "case statement outside of switch") return $
        listToMaybe [sid | SwitchID sid <- stack]

    x' <- resolveExpr x
    case x' of
      Lit x'' -> do
        existing <- use (switchCases . at controlID . non Set.empty)
        if Set.member x'' existing
          then throwError (IllegalCaseExpression "duplicate case")
          else do
            switchCases . at controlID %= Just . Set.insert x'' . fromMaybe Set.empty
            return $ Case controlID (Lit x'')
      _ -> throwError $ IllegalCaseExpression "non-constant case expressions are not allowed"
  DefaultP -> do
    stack <- view controlStack
    controlID <-
      maybe (throwError $ IllegalControlStatement "case statement outside of switch") return $
        listToMaybe [sid | SwitchID sid <- stack]

    existing <- use (switchDefault . at controlID . non False)
    if existing
      then throwError (IllegalDefault "maximum of one default per switch")
      else do
        switchDefault . at controlID .= Just True
        return (DefaultR controlID)
  _ -> error "ICE: unreachable"

resolveDecl :: Decl Parsed -> Transform (Decl Resolved)
resolveDecl (Decl d expr) = do
  sid <- view (scopeStack . to head)
  currentScopeMap <- use (varMap . at sid) <&> fromMaybe Map.empty
  when (Map.member d currentScopeMap) $ throwError (VariableRedefinition d)
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
resolveBlock (BlockP items) = scopeSeq <<+= 1 >>= \sid' -> withScope sid' $ Block sid' <$> resolveBlockItems items
resolveBlock _ = error "ICE: unreachable"

resolveFunc :: Func Parsed -> Transform (Func Resolved)
resolveFunc (Func ident funcBody) = local (func ?~ ident) $ Func ident <$> resolveBlock funcBody

resolveProgram :: Program Parsed -> Transform (Program Resolved)
resolveProgram (Program f) =
  scopeSeq <<+= 1 >>= \sid' ->
    withScope sid' (resolveFunc f >>= (populateProgramSwitchCases >=> validateProgramLabels) . Program)
