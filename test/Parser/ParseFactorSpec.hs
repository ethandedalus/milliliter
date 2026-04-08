module Parser.ParseFactorSpec where

import qualified CompileError as CE (CompileError (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)
import Lexer (runLexer)
import Lexer.Types (Token (..), mkSourceLoc)
import Parser.Combinators (parseFactor)
import Parser.Errors (unexpectedEOF)
import Parser.Types (BinaryOperator (..), Expr (..), Factor (..), UnaryOperator (..))
import Shared.UnitTest (UnitTest (UnitTest))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Factor

unaryComplement :: Test
unaryComplement =
  UnitTest "unary complement" "~42" $ Right $ Unary Complement (Lit (LiteralInt 42))

unaryNegation :: Test
unaryNegation =
  UnitTest "unary negation" "-42" $ Right $ Unary Negate (Lit (LiteralInt 42))

parenthesizedLiteralInt :: Test
parenthesizedLiteralInt =
  UnitTest "parenthesized literal int" "(42)" $ Right $ Expr (Factor (Lit (LiteralInt 42)))

nestedParenthesizedLiteralInt :: Test
nestedParenthesizedLiteralInt =
  UnitTest "nested parenthesized literal int" "(((42)))" $ Right $ Expr (Factor (Expr (Factor (Expr (Factor (Lit (LiteralInt 42)))))))

nestedParenthesizedUnaryOperators :: Test
nestedParenthesizedUnaryOperators =
  UnitTest "nested parenthesized unary operators" "-(~(42))" $
    Right $
      Unary Negate (Expr (Factor (Unary Complement (Expr (Factor (Lit (LiteralInt 42)))))))

unaryOnParenthesizedExpr :: Test
unaryOnParenthesizedExpr =
  UnitTest "unary on parenthesized expression" "~(7 * 6)" $
    Right $
      Unary Complement (Expr (Binary Mul (Factor (Lit (LiteralInt 7))) (Factor (Lit (LiteralInt 6)))))

doubleUnary :: Test
doubleUnary =
  UnitTest "double unary" "~-42" $
    Right $
      Unary Complement (Unary Negate (Lit (LiteralInt 42)))

incorrectlyParenthesizedLiteralInt :: Test
incorrectlyParenthesizedLiteralInt =
  UnitTest "incorrectly parenthesized literal int (missing right parenthesis)" "(42" $
    Left $
      CE.ParseError (unexpectedEOF (show TRParen))

spec :: Spec
spec = do
  describe "unary factor expressions" $ do
    let unaryFactorTestCases =
          [ unaryComplement
          , unaryNegation
          , parenthesizedLiteralInt
          , nestedParenthesizedLiteralInt
          , nestedParenthesizedUnaryOperators
          , unaryOnParenthesizedExpr
          , doubleUnary
          , incorrectlyParenthesizedLiteralInt
          ]

    forM_ unaryFactorTestCases $ \(UnitTest caseName input expectedResult) -> it ("case: " ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        first CE.ParseError $ evalStateT parseFactor tokens
