module Compiler.Codegen (codegen) where

import qualified Compiler.Codegen.ASM as ASM (lower, lowerProgram)
import qualified Compiler.Codegen.InstructionFixup as IFX (instructionFixup, lowerProgram)
import qualified Compiler.Codegen.PseudoRegisters as PRG (lower, lowerProgram)
import Compiler.Codegen.Types (Program (..))
import qualified Compiler.IR.Types as IR (Program (..))
import Compiler.Stage (Stage)
import Control.Monad ((>=>))

codegen :: Stage IR.Program Program
codegen =
  ASM.lower ASM.lowerProgram
    >=> PRG.lower PRG.lowerProgram
    >=> IFX.instructionFixup IFX.lowerProgram
