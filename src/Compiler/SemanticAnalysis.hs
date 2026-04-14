module Compiler.SemanticAnalysis (analyze, analyzeWithState) where

import qualified Compiler.Error as CE (CompileError (SemanticAnalysisError))
import Compiler.SemanticAnalysis.Types
import Compiler.Stage (Stage)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (Bifunctor (first))

analyzeWithState :: SemanticAnalysisState -> (i -> Transform a) -> Stage i a
analyzeWithState s f x = first CE.SemanticAnalysisError $ evalStateT (f x) s

analyze :: (i -> Transform a) -> Stage i a
analyze = analyzeWithState SemanticAnalysisState{_varSeq = 0, _varMap = mempty}
