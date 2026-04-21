{-# LANGUAGE DataKinds #-}

module Compiler.SemanticAnalysis (analyze, analyzeWithState, analyzeProgram) where

import Compiler.AST
import qualified Compiler.Error as CE (CompileError (SemanticAnalysisError))
import Compiler.Pass
import Compiler.SemanticAnalysis.Resolve (resolveProgram)
import Compiler.SemanticAnalysis.Types
import Compiler.Stage (Stage)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (Bifunctor (first))

analyzeWithState ::
  ScopeContext ->
  SemanticAnalysisState ->
  (i -> Transform a) ->
  Stage i a
analyzeWithState ctx s f x = first CE.SemanticAnalysisError $ evalStateT (runReaderT (f x) ctx) s

analyzeProgram :: Program Parsed -> Transform (Program Resolved)
analyzeProgram = resolveProgram

analyze :: (Program Parsed -> Transform (Program Resolved)) -> Stage (Program Parsed) (Program Resolved)
analyze = analyzeWithState ctx sas
 where
  ctx = ScopeContext{_scopeStack = [], _func = Nothing, _controlStack = []}
  sas =
    SemanticAnalysisState
      { _varSeq = 0
      , _varMap = mempty
      , _scopeSeq = 0
      , _funcLabels = mempty
      , _switchCases = mempty
      , _switchDefault = mempty
      , _controlSeq = 0
      }
