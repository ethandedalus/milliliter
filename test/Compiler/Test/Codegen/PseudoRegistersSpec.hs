module Compiler.Test.Codegen.PseudoRegistersSpec where

import qualified Compiler.Codegen.ASM as ASM (lower, lowerInstructions, lowerVal)
import qualified Compiler.Codegen.PseudoRegisters as PRG (lower, lowerInstructions, lowerVal)
import Compiler.Codegen.Types (Instruction (..), Operand (..), Register (..))
import qualified Compiler.IR as IR (lower)
import qualified Compiler.IR.Lower as IR (lowerExpr, lowerStmt)
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseExpr, parseStmt)
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

lowerLiteralInt :: UnitTest Operand
lowerLiteralInt = UnitTest "literal int" "42" compile $ Right $ Imm 42
 where
  compile =
    Lexer.lex
      >=> P.parse (P.parseExpr 0)
      >=> IR.lower IR.lowerExpr
      >=> (return . snd)
      >=> ASM.lower ASM.lowerVal
      >=> PRG.lower PRG.lowerVal

lowerReturnLiteral :: UnitTest [Instruction]
lowerReturnLiteral = UnitTest "return literal" "return 42;" compile $ Right [Mov (Imm 42) (Register AX), Ret]
 where
  compile =
    Lexer.lex
      >=> P.parse P.parseStmt
      >=> IR.lower IR.lowerStmt
      >=> ASM.lower ASM.lowerInstructions
      >=> PRG.lower PRG.lowerInstructions

spec :: Spec
spec = do
  describe "lower value" $ do
    let lowerValueTestCases = [lowerLiteralInt]

    forM_ lowerValueTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "lower instruction" $ do
    let lowerInstructionTestCases = [lowerReturnLiteral]

    forM_ lowerInstructionTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
