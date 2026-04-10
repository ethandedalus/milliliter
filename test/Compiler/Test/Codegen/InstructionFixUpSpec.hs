module Compiler.Test.Codegen.InstructionFixUpSpec where

import qualified Compiler.Codegen.ASM as ASM (lower, lowerInstructions)
import qualified Compiler.Codegen.InstructionFixup as IFX (lower, lowerInstructions)
import qualified Compiler.Codegen.PseudoRegisters as PRG (lower, lowerInstructions)
import Compiler.Codegen.Types (BinaryOperator (..), Instruction (..), Operand (..), Register (..))
import qualified Compiler.IR as IR (lower)
import qualified Compiler.IR.Lower as IR (lowerExpr)
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseExpr)
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

lowerExpressionWithShl1 :: UnitTest [Instruction]
lowerExpressionWithShl1 = UnitTest "return shl expression" "(2 * 5) << 2" compile result
 where
  compile =
    Lexer.lex
      >=> P.parse (P.parseExpr 0)
      >=> IR.lower IR.lowerExpr
      >=> (return . fst)
      >=> ASM.lower ASM.lowerInstructions
      >=> PRG.lower PRG.lowerInstructions
      >=> IFX.lower IFX.lowerInstructions

  result =
    pure
      [ Mov (Imm 2) (Stack (-4))
      , Mov (Stack (-4)) (Register R11)
      , Binary Mul (Imm 5) (Register R11)
      , Mov (Register R11) (Stack (-4))
      , Mov (Stack (-4)) (Register R10)
      , Mov (Register R10) (Stack (-8))
      , Mov (Imm 2) (Register CX)
      , Binary LeftShift (Register CL) (Stack (-8))
      ]

lowerExpressionWithShl2 :: UnitTest [Instruction]
lowerExpressionWithShl2 = UnitTest "shl" "2 * 5 << 1 + 2" compile result
 where
  compile =
    Lexer.lex
      >=> P.parse (P.parseExpr 0)
      >=> IR.lower IR.lowerExpr
      >=> (return . fst)
      >=> ASM.lower ASM.lowerInstructions
      >=> PRG.lower PRG.lowerInstructions
      >=> IFX.lower IFX.lowerInstructions

  result =
    pure
      [ Mov (Imm 2) (Stack (-4))
      , Mov (Stack (-4)) (Register R11)
      , Binary Mul (Imm 5) (Register R11)
      , Mov (Register R11) (Stack (-4)) -- 2 * 5 is now in -4(%rbp)
      , Mov (Imm 1) (Stack (-8))
      , Binary Add (Imm 2) (Stack (-8)) -- 1 + 2 is now in -8(%rbp)
      , Mov (Stack (-4)) (Register R10) -- 10 is now in %r10d
      , Mov (Register R10) (Stack (-12)) -- 10 is now in -12(%rbp)
      , Mov (Stack (-8)) (Register CX) -- 3 is now in %ecx
      , Binary LeftShift (Register CL) (Stack (-12)) -- 80 is now in -12(%rbp)
      ]

spec :: Spec
spec = do
  describe "lower left shift expressions" $ do
    let lowerValueTestCases = [lowerExpressionWithShl1, lowerExpressionWithShl2]

    forM_ lowerValueTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
