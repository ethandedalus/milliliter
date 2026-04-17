{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.SemanticAnalysis.LabelValidation where

import Compiler.AST
import Compiler.Pass
import Compiler.SemanticAnalysis.Types
import Control.Lens
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (empty, member)

validateStmt :: Stmt Resolved -> Transform (Stmt Resolved)
validateStmt = \case
  g@(Goto enclosingFunc label) -> do
    exists <- uses funcLabels (Set.member label . fromMaybe Set.empty . Map.lookup enclosingFunc)
    unless exists $ throwError (IllegalLabel label ("does not exist in " ++ enclosingFunc))
    return g
  other -> return other

validateBlockItem :: BlockItem Resolved -> Transform (BlockItem Resolved)
validateBlockItem = \case
  S stmt -> S <$> validateStmt stmt
  other -> pure other

validateBlock :: Block Resolved -> Transform (Block Resolved)
validateBlock (Block sid items) = Block sid <$> mapM validateBlockItem items

validateFuncLabels :: Func Resolved -> Transform (Func Resolved)
validateFuncLabels (Func ident block) = Func ident <$> validateBlock block

validateProgramLabels :: Program Resolved -> Transform (Program Resolved)
validateProgramLabels (Program f) = Program <$> validateFuncLabels f
