module Compiler.Test.Codegen.CodegenExprSpec where

import qualified Compiler.Codegen.ASM as ASM (lower, lowerInstructions)
import qualified Compiler.Codegen.InstructionFixup as IFX (lower, lowerInstructions)
import qualified Compiler.Codegen.PseudoRegisters as PRG (lower, lowerInstructions)
import Compiler.Codegen.Types (BinaryOperator (..), CondCode (..), Instruction (..), Operand (..), Register (..))
import Compiler.Error (CompileError)
import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformExpr)
import qualified Compiler.IR.Types as IR (Instruction (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseExpr)
import qualified Compiler.SemanticAnalysis as S
import qualified Compiler.SemanticAnalysis.Resolve as S
import qualified Compiler.SemanticAnalysis.Types as S
import Compiler.Stage (Stage)
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest), prepareEnv)
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

lower :: Stage [IR.Instruction] [Instruction]
lower = ASM.lower ASM.lowerInstructions >=> PRG.lower PRG.lowerInstructions >=> IFX.lower IFX.lowerInstructions

compile ::
  S.ScopeContext ->
  S.SemanticAnalysisState ->
  String ->
  Either CompileError [Instruction]
compile ctx sas =
  Lexer.lex
    >=> P.parse (P.parseExpr 0)
    >=> analyze
    >=> IR.transform IR.transformExpr
    >=> (return . fst)
    >=> lower
 where
  analyze = S.analyzeWithState ctx sas S.resolveExpr

spec :: Spec
spec = do
  describe "bitwise" $ do
    let bitwiseTestCases =
          [ shl1
          , shl2
          , sar1
          , bitwisePrecedence1
          ]

    forM_ bitwiseTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "arithmetic" $ do
    let lowerRightShiftExpressionTestCases = [addition1, addition2, composite1]

    forM_ lowerRightShiftExpressionTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "short circuiting" $ do
    let shortCircuitingExpressionTestCases = [lowerLogicalAnd1, lowerLogicalOr1]

    forM_ shortCircuitingExpressionTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

-- describe "relational" $ do
--   let shortCircuitingExpressionTestCases = [lowerGreaterThan1]
--
--   forM_ shortCircuitingExpressionTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
--     run input `shouldBe` expectedResult
--
-- describe "compound" $ do
--   let testCases = [compound1, compound2]
--
--   forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
--     run input `shouldBe` expectedResult

shl1 :: UnitTest [Instruction]
shl1 = UnitTest "return shl expression" "(2 * 5) << 2" (compile ctx sas) result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    pure
      [ Mov (Imm 2) (Stack (-4))
      , Mov (Stack (-4)) (Register R11)
      , Binary Mul (Imm 5) (Register R11)
      , Mov (Register R11) (Stack (-4))
      , Mov (Stack (-4)) (Register R10)
      , Mov (Register R10) (Stack (-8))
      , Mov (Imm 2) (Register CX)
      , Binary LeftShift (Register CX) (Stack (-8))
      ]

shl2 :: UnitTest [Instruction]
shl2 = UnitTest "shl" "2 * 5 << 1 + 2" (compile ctx sas) result
 where
  (ctx, sas, _) = prepareEnv []
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
      , Binary LeftShift (Register CX) (Stack (-12)) -- 80 is now in -12(%rbp)
      ]

--
sar1 :: UnitTest [Instruction]
sar1 = UnitTest "sar" "32 + 4 * 8 >> 3" (compile ctx sas) result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    pure
      [ Mov (Imm 4) (Stack (-4))
      , Mov (Stack (-4)) (Register R11)
      , Binary Mul (Imm 8) (Register R11)
      , Mov (Register R11) (Stack (-4))
      , Mov (Imm 32) (Stack (-8))
      , Mov (Stack (-4)) (Register R10)
      , Binary Add (Register R10) (Stack (-8))
      , Mov (Stack (-8)) (Register R10)
      , Mov (Register R10) (Stack (-12))
      , Mov (Imm 3) (Register CX)
      , Binary RightShift (Register CX) (Stack (-12))
      ]

bitwisePrecedence1 :: UnitTest [Instruction]
bitwisePrecedence1 = UnitTest "precedence" "80 >> 2 | 1 ^ 5 & 7 << 1" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []

  result =
    [ Mov (Imm 80) (Stack (-4))
    , Mov (Imm 2) (Register CX)
    , Binary RightShift (Register CX) (Stack (-4))
    , Mov (Imm 7) (Stack (-8))
    , Mov (Imm 1) (Register CX)
    , Binary LeftShift (Register CX) (Stack (-8))
    , Mov (Imm 5) (Stack (-12))
    , Mov (Stack (-8)) (Register R12)
    , Binary BitAnd (Register R12) (Stack (-12))
    , Mov (Imm 1) (Stack (-16))
    , Mov (Stack (-12)) (Register R12)
    , Binary Xor (Register R12) (Stack (-16))
    , Mov (Stack (-4)) (Register R10)
    , Mov (Register R10) (Stack (-20))
    , Mov (Stack (-16)) (Register R12)
    , Binary BitOr (Register R12) (Stack (-20))
    ]

addition1 :: UnitTest [Instruction]
addition1 = UnitTest "addition (1)" "21 + 21" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Imm 21) (Stack (-4))
    , Binary Add (Imm 21) (Stack (-4))
    ]

addition2 :: UnitTest [Instruction]
addition2 = UnitTest "addition (2)" "1 + 2 + 3 + 4 + 5" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Imm 1) (Stack (-4)) -- movl $1, -4(%rbp) # 1 is in a
    , Binary Add (Imm 2) (Stack (-4)) -- addl $2, -4(%rbp) # 3 is in a
    , Mov (Stack (-4)) (Register R10) -- movl -4(%rbp), %r10d # 3 is in %r10d
    , Mov (Register R10) (Stack (-8)) -- movl %r10d, -8(%rbp) # 3 is in -8(%rbp)
    , Binary Add (Imm 3) (Stack (-8)) -- addl $3, -8(%rbp) # 6 is in b
    , Mov (Stack (-8)) (Register R10) -- movl -8(%rbp), %r10d # 6 is in %r10d
    , Mov (Register R10) (Stack (-12)) -- movl %r10d, -12(%rbp) # 6 is in c
    , Binary Add (Imm 4) (Stack (-12)) -- addl $4, -12(%rbp) # 10 is in c
    , Mov (Stack (-12)) (Register R10) -- movl -12(%rbp), %r10d # 10 is in %r10d
    , Mov (Register R10) (Stack (-16)) -- movl %r10d, -16(%rbp) # 10 is in d
    , Binary Add (Imm 5) (Stack (-16)) -- addl $5, -16(%rbp)  # 15 is in d
    ]

composite1 :: UnitTest [Instruction]
composite1 = UnitTest "composite (1)" "1 + (2 * 3) + 4 / 5 + 15 % 4" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Imm 2) (Stack (-4))
    , Mov (Stack (-4)) (Register R11)
    , Binary Mul (Imm 3) (Register R11)
    , Mov (Register R11) (Stack (-4))
    , Mov (Imm 1) (Stack (-8))
    , Mov (Stack (-4)) (Register R10)
    , Binary Add (Register R10) (Stack (-8))
    , Mov (Imm 4) (Register AX)
    , CDQ
    , Mov (Imm 5) (Register R10)
    , IDiv (Register R10)
    , Mov (Register AX) (Stack (-12))
    , Mov (Stack (-8)) (Register R10)
    , Mov (Register R10) (Stack (-16))
    , Mov (Stack (-12)) (Register R10)
    , Binary Add (Register R10) (Stack (-16))
    , Mov (Imm 15) (Register AX)
    , CDQ
    , Mov (Imm 4) (Register R10)
    , IDiv (Register R10)
    , Mov (Register DX) (Stack (-20))
    , Mov (Stack (-16)) (Register R10)
    , Mov (Register R10) (Stack (-24))
    , Mov (Stack (-20)) (Register R10)
    , Binary Add (Register R10) (Stack (-24))
    ]

lowerLogicalAnd1 :: UnitTest [Instruction]
lowerLogicalAnd1 = UnitTest "logical && (1)" "1 && 1" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Imm 1) (Register R11) -- movl $1, %r11d
    , Cmp (Imm 0) (Register R11) -- cmpl $0, %r11d
    , JmpCC CondE "_false0" -- je _false0
    , Mov (Imm 1) (Register R11) -- movl $1, %r11d
    , Cmp (Imm 0) (Register R11) -- cmpl $0, %r11d
    , JmpCC CondE "_false0" -- je _false0
    , Mov (Imm 1) (Stack (-4)) -- mov $1, -4(%rbp)
    , Jmp "_end1" -- jmp _end1
    , Label "_false0" -- _false0:
    , Mov (Imm 0) (Stack (-4)) -- movl $0, -4(%rbp)
    , Label "_end1" -- _end1:
    ]

lowerLogicalOr1 :: UnitTest [Instruction]
lowerLogicalOr1 = UnitTest "logical || (1)" "0 || 1" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Imm 0) (Register R11) -- movl $0, %r11d
    , Cmp (Imm 0) (Register R11) -- cmpl $0, %r11d
    , JmpCC CondNE "_true0" -- jne _true0
    , Mov (Imm 1) (Register R11) -- movl $1, %r11d
    , Cmp (Imm 0) (Register R11) -- cmpl $0, %r11d
    , JmpCC CondNE "_true0" -- jne _true0
    , Mov (Imm 0) (Stack (-4)) -- mov $0, -4(%rbp)
    , Jmp "_end1" -- jmp _end1
    , Label "_true0" -- _true0:
    , Mov (Imm 1) (Stack (-4)) -- movl $1, -4(%rbp)
    , Label "_end1" -- _end1:
    ]

lowerGreaterThan1 :: UnitTest [Instruction]
lowerGreaterThan1 = UnitTest "relational > (1)" "2 > 1" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Imm 2) (Register R11) -- movl $2, %r11d
    , Cmp (Imm 1) (Register R11) -- cmpl $1, %r11d
    , Mov (Imm 0) (Stack (-4)) -- movl $0, -4(%rbp)
    , SetCC CondG (Stack (-4)) -- setg, -4(%rbp)
    ]

compound1 :: UnitTest [Instruction]
compound1 = UnitTest "compound assignment" "a += b" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Stack (-4)) (Register R10) -- movl -4(%rbp), %r10d
    , Mov (Register R10) (Stack (-4)) -- movl %r10d,
    , Mov (Stack (-8)) (Register R10)
    , Binary Add (Register R10) (Stack (-4))
    ]

compound2 :: UnitTest [Instruction]
compound2 = UnitTest "compound assignment" "a += 1" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Mov (Stack (-4)) (Register R10)
    , Mov (Register R10) (Stack (-4))
    , Binary Add (Imm 1) (Stack (-4))
    ]
