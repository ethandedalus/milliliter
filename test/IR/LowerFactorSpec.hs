module IR.LowerFactorSpec where

import qualified CompileError as CE (CompileError (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)
import IR.Emit (lowerFactor)
import IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import Lexer (runLexer)
import Lexer.Types (mkSourceLoc)
import Parser.Combinators (parseFactor)
import Shared.UnitTest (UnitTest (..))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ([Instruction], Val)

unitTestLiteralInt :: Test
unitTestLiteralInt = UnitTest "literal int" "42" $ Right ([], Lit (LiteralInt 42))

unitTestUnaryOperation :: Test
unitTestUnaryOperation = UnitTest "unary operation" "~42" $ Right ([Unary Complement (Lit $ LiteralInt 42) (Var "tmp.0")], Var "tmp.0")

unitTestUnaryOperationParenthesized :: UnitTest ([Instruction], Val)
unitTestUnaryOperationParenthesized =
  UnitTest "unary operation (parenthesized)" "~(42)" $
    Right ([Unary Complement (Lit (LiteralInt 42)) (Var "tmp.0")], Var "tmp.0")

unitTestMultipleUnaryOperations :: Test
unitTestMultipleUnaryOperations =
  UnitTest "multiple unary operations" "~(-42)" $
    Right
      (
        [ Unary Negate (Lit (LiteralInt 42)) (Var "tmp.0")
        , Unary Complement (Var "tmp.0") (Var "tmp.1")
        ]
      , Var "tmp.1"
      )

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
    forM_ lowerFactorTestCases $ \(UnitTest caseName input expectedResult) -> it ("case: " ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        factor <- first CE.ParseError $ evalStateT parseFactor tokens
        first CE.IRError $ evalStateT (lowerFactor factor) 0
