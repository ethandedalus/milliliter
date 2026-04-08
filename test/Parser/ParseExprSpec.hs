module Parser.ParseExprSpec where

import qualified CompileError as CE (CompileError (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)
import Lexer (runLexer)
import Lexer.Types (mkSourceLoc)
import Parser.Combinators (parseExpr)
import Parser.Types (BinaryOperator (..), Expr (..), Factor (..), UnaryOperator (..))
import Shared.UnitTest (UnitTest (UnitTest))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Expr

literalInt :: Test
literalInt = UnitTest "literal int" "42" $ Right $ Factor (Lit (LiteralInt 42))

unaryFactor1 :: Test
unaryFactor1 = UnitTest "unary factor expression" "~42" $ Right $ Factor (Unary Complement (Lit (LiteralInt 42)))

unaryFactor2 :: Test
unaryFactor2 = UnitTest "unary factor expression" "~(42)" $ Right $ Factor (Unary Complement (Expr (Factor (Lit (LiteralInt 42)))))

simpleAddition :: Test
simpleAddition =
  UnitTest "simple 2 term arithmetic" "21 + 21" $
    Right $
      Binary Add (Factor (Lit (LiteralInt 21))) (Factor (Lit (LiteralInt 21)))

arith1 :: Test
arith1 =
  UnitTest "simple 3 term arithmetic" "21 + 3 * 7" $
    Right $
      Binary Add (Factor (Lit (LiteralInt 21))) $
        Binary Mul (Factor (Lit (LiteralInt 3))) (Factor (Lit (LiteralInt 7)))

arithWithGrouping1 :: Test
arithWithGrouping1 =
  UnitTest "arithmetic with grouping (1)" "10 * 3 / (7 + 11)" $
    Right
      ( Binary
          Div
          (Binary Mul (Factor (Lit (LiteralInt 10))) (Factor (Lit (LiteralInt 3))))
          (Factor (Expr (Binary Add (Factor (Lit (LiteralInt 7))) (Factor (Lit (LiteralInt 11))))))
      )

arithWithGrouping2 :: Test
arithWithGrouping2 =
  UnitTest "arithmetic with grouping (2)" "10 + 5 * (2 + 5) - 3" $
    Right
      ( Binary
          Sub
          ( Binary
              Add
              (Factor (Lit (LiteralInt 10)))
              (Binary Mul (Factor (Lit (LiteralInt 5))) (Factor (Expr (Binary Add (Factor (Lit (LiteralInt 2))) (Factor (Lit (LiteralInt 5)))))))
          )
          (Factor (Lit (LiteralInt 3)))
      )

spec :: Spec
spec = do
  describe "parse literal expressions" $ do
    let parseLiteralTestCases = [literalInt]

    forM_ parseLiteralTestCases $ \(UnitTest caseName input expectedResult) -> it ("case:" ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        first CE.ParseError $ evalStateT (parseExpr 0) tokens

  describe "parse unary expressions" $ do
    let parseUnaryExpressionTestCases = [unaryFactor1, unaryFactor2]

    forM_ parseUnaryExpressionTestCases $ \(UnitTest caseName input expectedResult) -> it ("case:" ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        first CE.ParseError $ evalStateT (parseExpr 0) tokens

  describe "parse binary expressions" $ do
    let parseBinaryExpressionTestCases = [simpleAddition, arith1, arithWithGrouping1, arithWithGrouping2]

    forM_ parseBinaryExpressionTestCases $ \(UnitTest caseName input expectedResult) -> it ("case:" ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        first CE.ParseError $ evalStateT (parseExpr 0) tokens
