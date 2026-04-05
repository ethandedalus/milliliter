module Codegen.ASM.ASMSpec where

import Codegen.ASM (lowerInstruction, lowerVal)
import Codegen.Types (Instruction (..), Operand (..), Register (..), UnaryOperator (..))
import qualified Compiler.Types as C (Literal (..))
import qualified Compiler.Types as CT (Literal (LiteralInt))
import Control.Monad (forM_)
import qualified IR.Types as IR
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "lower value" $ do
    let cases =
          [ ("literal int", IR.Lit (CT.LiteralInt 42), Right $ Imm 42),
            ("variable", IR.Var "tmp.0", Right $ Pseudo "tmp.0")
          ]

    forM_ cases $ \(name, input, expected) -> it ("case: " ++ name) $ lowerVal input `shouldBe` expected

  describe "lower instruction" $ do
    let cases =
          [ ("return literal", IR.Return (IR.Lit (C.LiteralInt 42)), Right [Mov (Imm 42) (Register AX), Ret]),
            ( "unary operation",
              IR.Unary IR.Complement (IR.Lit (C.LiteralInt 42)) (IR.Var "tmp.0"),
              Right [Mov (Imm 42) (Pseudo "tmp.0"), Unary Complement (Pseudo "tmp.0")]
            )
          ]

    forM_ cases $ \(name, input, expected) -> it ("case: " ++ name) $ lowerInstruction input `shouldBe` expected
