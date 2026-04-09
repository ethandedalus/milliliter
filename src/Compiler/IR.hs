module Compiler.IR (lower) where

import qualified Compiler.Error as CE (CompileError (IRError))
import Compiler.IR.Types (Emitter)
import Compiler.Stage (Stage)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)

lower :: (i -> Emitter a) -> Stage i a
lower f input = first CE.IRError $ evalStateT (f input) 0
