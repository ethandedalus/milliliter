module Compiler.IR (transform) where

import qualified Compiler.Error as CE (CompileError (IRError))
import Compiler.IR.Types (IRState (..), Transform)
import Compiler.Stage (Stage)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)

transform :: (i -> Transform a) -> Stage i a
transform f input = first CE.IRError $ evalStateT (f input) (IRState{_varSeq = 0, _labelSeq = 0})
