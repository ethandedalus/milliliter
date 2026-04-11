module Compiler.Test.Parser.ParseExprSpec where

import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseExpr)
import Compiler.Parser.Types (BinaryOperator (..), Expr (..), Factor (..), UnaryOperator (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Compiler.Types (Literal (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Expr

spec :: Spec
spec = do
  describe "parse literal expressions" $ do
    let parseLiteralTestCases = [literalInt]

    forM_ parseLiteralTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "parse unary expressions" $ do
    let parseUnaryExpressionTestCases = [unaryFactor1, unaryFactor2]

    forM_ parseUnaryExpressionTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "parse binary expressions" $ do
    let parseBinaryExpressionTestCases = [simpleAddition, arith1, arithWithGrouping1, arithWithGrouping2]

    forM_ parseBinaryExpressionTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

literalInt :: Test
literalInt = UnitTest "literal int" "42" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Factor (Lit (LiteralInt 42))

unaryFactor1 :: Test
unaryFactor1 = UnitTest "unary factor expression" "~42" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Factor (Unary Complement (Lit (LiteralInt 42)))

unaryFactor2 :: Test
unaryFactor2 = UnitTest "unary factor expression" "~(42)" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Factor (Unary Complement (Expr (Factor (Lit (LiteralInt 42)))))

simpleAddition :: Test
simpleAddition = UnitTest "simple 2 term arithmetic" "21 + 21" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Binary Add (Factor (Lit (LiteralInt 21))) (Factor (Lit (LiteralInt 21)))

arith1 :: Test
arith1 =
  UnitTest "simple 3 term arithmetic" "21 + 3 * 7" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result =
    Binary Add (Factor (Lit (LiteralInt 21))) $
      Binary Mul (Factor (Lit (LiteralInt 3))) (Factor (Lit (LiteralInt 7)))

arithWithGrouping1 :: Test
arithWithGrouping1 =
  UnitTest "arithmetic with grouping (1)" "10 * 3 / (7 + 11)" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result =
    Binary
      Div
      (Binary Mul (Factor (Lit (LiteralInt 10))) (Factor (Lit (LiteralInt 3))))
      (Factor (Expr (Binary Add (Factor (Lit (LiteralInt 7))) (Factor (Lit (LiteralInt 11))))))

arithWithGrouping2 :: Test
arithWithGrouping2 = UnitTest "arithmetic with grouping (2)" "10 + 5 * (2 + 5) - 3" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result =
    Binary
      Sub
      ( Binary
          Add
          (Factor (Lit (LiteralInt 10)))
          (Binary Mul (Factor (Lit (LiteralInt 5))) (Factor (Expr (Binary Add (Factor (Lit (LiteralInt 2))) (Factor (Lit (LiteralInt 5)))))))
      )
      (Factor (Lit (LiteralInt 3)))
