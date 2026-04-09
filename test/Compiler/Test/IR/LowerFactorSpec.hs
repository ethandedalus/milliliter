module Compiler.Test.IR.LowerFactorSpec where

import qualified Compiler.IR as IR (lower)
import qualified Compiler.IR.Lower as IR (lowerFactor)
import Compiler.IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseFactor)
import Compiler.Test.Shared.UnitTest (UnitTest (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (lex)

type Test = UnitTest ([Instruction], Val)

unitTestLiteralInt :: Test
unitTestLiteralInt = UnitTest "literal int" "42" run $ Right ([], Lit (LiteralInt 42))
 where
  run = Lexer.lex >=> P.parse P.parseFactor >=> IR.lower IR.lowerFactor

unitTestUnaryOperation :: Test
unitTestUnaryOperation = UnitTest "unary operation" "~42" run $ Right ([Unary Complement (Lit $ LiteralInt 42) (Var "tmp.0")], Var "tmp.0")
 where
  run = Lexer.lex >=> P.parse P.parseFactor >=> IR.lower IR.lowerFactor

unitTestUnaryOperationParenthesized :: Test
unitTestUnaryOperationParenthesized =
  UnitTest "unary operation (parenthesized)" "~(42)" run $
    Right ([Unary Complement (Lit (LiteralInt 42)) (Var "tmp.0")], Var "tmp.0")
 where
  run = Lexer.lex >=> P.parse P.parseFactor >=> IR.lower IR.lowerFactor

unitTestMultipleUnaryOperations :: Test
unitTestMultipleUnaryOperations =
  UnitTest "multiple unary operations" "~(-42)" run $
    Right
      (
        [ Unary Negate (Lit (LiteralInt 42)) (Var "tmp.0")
        , Unary Complement (Var "tmp.0") (Var "tmp.1")
        ]
      , Var "tmp.1"
      )
 where
  run = Lexer.lex >=> P.parse P.parseFactor >=> IR.lower IR.lowerFactor

lowerFactorTestCases :: [Test]
lowerFactorTestCases =
  [ unitTestLiteralInt
  , unitTestUnaryOperation
  , unitTestUnaryOperationParenthesized
  , unitTestMultipleUnaryOperations
  ]

spec :: Spec
spec = do
  describe "lower factor" $ do
    forM_ lowerFactorTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
