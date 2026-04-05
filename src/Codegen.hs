module Codegen (lower) where

import qualified Codegen.ASM as ASM (lower)
import qualified Codegen.InstructionFixup as IFX (lower)
import qualified Codegen.PseudoRegisters as PRG (lower)
import Codegen.Types (CodegenError (..), Program (..), PseudoRegistersPassState (..))
import Control.Monad.State (evalStateT)
import qualified IR.Types as IR (Program (..))

lower :: IR.Program -> Either CodegenError Program
lower program = do
  p <- ASM.lower program
  p' <- evalStateT (PRG.lower p) (PseudoRegistersPassState mempty 0)
  pure (IFX.lower p')
