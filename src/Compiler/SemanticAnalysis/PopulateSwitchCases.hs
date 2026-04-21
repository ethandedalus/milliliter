{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.SemanticAnalysis.PopulateSwitchCases where

import Compiler.AST
import Compiler.Pass
import Compiler.SemanticAnalysis.Types
import Control.Lens hiding (Empty)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (empty, member, toList)

populateBlockItemSwitchCases :: BlockItem Resolved -> Transform (BlockItem Resolved)
populateBlockItemSwitchCases = \case
  S s -> S <$> populateStmtSwitchCases s
  other -> return other

populateBlockSwitchCases :: Block Resolved -> Transform (Block Resolved)
populateBlockSwitchCases (Block sid items) = Block sid <$> mapM populateBlockItemSwitchCases items

populateStmtSwitchCases :: Stmt Resolved -> Transform (Stmt Resolved)
populateStmtSwitchCases = \case
  Goto enclosingFunc label -> do
    exists <- uses funcLabels (Set.member label . fromMaybe Set.empty . Map.lookup enclosingFunc)
    unless exists $ throwError (IllegalLabel label ("does not exist in " ++ enclosingFunc))
    return (Goto enclosingFunc label)
  If x y z -> If x <$> populateStmtSwitchCases y <*> traverse populateStmtSwitchCases z
  Compound b -> Compound <$> populateBlockSwitchCases b
  WhileR x y z -> WhileR x y <$> populateStmtSwitchCases z
  DoWhileR x y z -> DoWhileR x <$> populateStmtSwitchCases y <*> return z
  ForR a b c d e -> For a b <$> traverse return c <*> traverse return d <*> populateStmtSwitchCases e
  SwitchR (SwitchData i _ _) y z -> do
    allCases <- use (switchCases . at i . non Set.empty . to Set.toList)
    hasDefault' <- use (switchDefault . at i . non False)
    Switch (SwitchData i allCases hasDefault') y <$> populateStmtSwitchCases z
  other -> return other

populateFuncSwitchCases :: Func Resolved -> Transform (Func Resolved)
populateFuncSwitchCases (Func ident block) = Func ident <$> populateBlockSwitchCases block

populateProgramSwitchCases :: Program Resolved -> Transform (Program Resolved)
populateProgramSwitchCases (Program f) = Program <$> populateFuncSwitchCases f
