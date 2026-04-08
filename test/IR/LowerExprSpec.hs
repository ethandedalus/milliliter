module IR.LowerExprSpec where

import qualified CompileError as CE (CompileError (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)
import IR.Emit (lowerExpr)
import IR.Types (BinaryOperator (..), Instruction (..), Val (..))
import Lexer (runLexer)
import Lexer.Types (mkSourceLoc)
import Parser.Combinators (parseExpr)
import Shared.UnitTest (UnitTest (..))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ([Instruction], Val)

lowerSimpleBinaryAddition :: Test
lowerSimpleBinaryAddition =
  UnitTest "simple binary expression" "1 + 1" $
    Right ([Binary Add (Lit (LiteralInt 1)) (Lit (LiteralInt 1)) (Var "tmp.0")], Var "tmp.0")

lowerParenthesizedBinaryAddition :: Test
lowerParenthesizedBinaryAddition =
  UnitTest "parenthesized binary addition" "(1 + 1)" $
    Right ([Binary Add (Lit (LiteralInt 1)) (Lit (LiteralInt 1)) (Var "tmp.0")], Var "tmp.0")

lowerGeneralArithmetic :: Test
lowerGeneralArithmetic =
  UnitTest "general arithmetic expression" "1 + 2 * (3 - 4) / 5" $
    Right
      (
        [ Binary Sub (Lit (LiteralInt 3)) (Lit (LiteralInt 4)) (Var "tmp.0")
        , Binary Mul (Lit (LiteralInt 2)) (Var "tmp.0") (Var "tmp.1")
        , Binary Div (Var "tmp.1") (Lit (LiteralInt 5)) (Var "tmp.2")
        , Binary Add (Lit (LiteralInt 1)) (Var "tmp.2") (Var "tmp.3")
        ]
      , Var "tmp.3"
      )

spec :: Spec
spec = do
  describe "lower binary expressions" $ do
    let binaryExpressionTestCases =
          [ lowerSimpleBinaryAddition
          , lowerParenthesizedBinaryAddition
          , lowerGeneralArithmetic
          ]

    forM_ binaryExpressionTestCases $ \(UnitTest caseName input expectedResult) -> it ("case: " ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        factor <- first CE.ParseError $ evalStateT (parseExpr 0) tokens
        first CE.IRError $ evalStateT (lowerExpr factor) 0
