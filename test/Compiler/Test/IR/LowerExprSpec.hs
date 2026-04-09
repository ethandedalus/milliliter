module Compiler.Test.IR.LowerExprSpec where

import qualified Compiler.IR as IR (lower)
import qualified Compiler.IR.Lower as IR (lowerExpr)
import Compiler.IR.Types (BinaryOperator (..), Instruction (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseExpr)
import Compiler.Test.Shared.UnitTest (UnitTest (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ([Instruction], Val)

lowerSimpleBinaryAddition :: Test
lowerSimpleBinaryAddition =
  UnitTest "simple binary expression" "1 + 1" run $
    Right ([Binary Add (Lit (LiteralInt 1)) (Lit (LiteralInt 1)) (Var "tmp.0")], Var "tmp.0")
 where
  run = Lexer.lex >=> P.parse (P.parseExpr 0) >=> IR.lower IR.lowerExpr

lowerParenthesizedBinaryAddition :: Test
lowerParenthesizedBinaryAddition =
  UnitTest "parenthesized binary addition" "(1 + 1)" run $
    Right ([Binary Add (Lit (LiteralInt 1)) (Lit (LiteralInt 1)) (Var "tmp.0")], Var "tmp.0")
 where
  run = Lexer.lex >=> P.parse (P.parseExpr 0) >=> IR.lower IR.lowerExpr

lowerGeneralArithmetic :: Test
lowerGeneralArithmetic =
  UnitTest "general arithmetic expression" "1 + 2 * (3 - 4) / 5" run $
    Right
      (
        [ Binary Sub (Lit (LiteralInt 3)) (Lit (LiteralInt 4)) (Var "tmp.0")
        , Binary Mul (Lit (LiteralInt 2)) (Var "tmp.0") (Var "tmp.1")
        , Binary Div (Var "tmp.1") (Lit (LiteralInt 5)) (Var "tmp.2")
        , Binary Add (Lit (LiteralInt 1)) (Var "tmp.2") (Var "tmp.3")
        ]
      , Var "tmp.3"
      )
 where
  run = Lexer.lex >=> P.parse (P.parseExpr 0) >=> IR.lower IR.lowerExpr

spec :: Spec
spec = do
  describe "lower binary expressions" $ do
    let binaryExpressionTestCases =
          [ lowerSimpleBinaryAddition
          , lowerParenthesizedBinaryAddition
          , lowerGeneralArithmetic
          ]

    forM_ binaryExpressionTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
