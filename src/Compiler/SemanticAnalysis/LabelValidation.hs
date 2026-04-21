{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.SemanticAnalysis.LabelValidation where

import Compiler.AST
import Compiler.Pass
import Compiler.SemanticAnalysis.Types
import Control.Lens hiding (Empty)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (empty, member)

validateBlockItem :: BlockItem Resolved -> Transform (BlockItem Resolved)
validateBlockItem = \case
  S s -> S <$> validateStmt s
  other -> return other

validateBlock :: Block Resolved -> Transform (Block Resolved)
validateBlock (Block sid items) = Block sid <$> mapM validateBlockItem items

validateStmt :: Stmt Resolved -> Transform (Stmt Resolved)
validateStmt = \case
  Goto enclosingFunc label -> do
    exists <- uses funcLabels (Set.member label . fromMaybe Set.empty . Map.lookup enclosingFunc)
    unless exists $ throwError (IllegalLabel label ("does not exist in " ++ enclosingFunc))
    return (Goto enclosingFunc label)
  If x y z -> If x <$> validateStmt y <*> traverse validateStmt z
  Compound b -> Compound <$> validateBlock b
  WhileR x y z -> WhileR x y <$> validateStmt z
  DoWhileR x y z -> DoWhileR x <$> validateStmt y <*> return z
  ForR a b c d e -> For a b <$> traverse return c <*> traverse return d <*> validateStmt e
  Switch x y z -> Switch x y <$> validateStmt z
  other -> return other

validateFuncLabels :: Func Resolved -> Transform (Func Resolved)
validateFuncLabels (Func ident block) = Func ident <$> validateBlock block

validateProgramLabels :: Program Resolved -> Transform (Program Resolved)
validateProgramLabels (Program f) = Program <$> validateFuncLabels f
