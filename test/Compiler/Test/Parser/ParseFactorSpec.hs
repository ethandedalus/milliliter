module Compiler.Test.Parser.ParseFactorSpec where

import qualified Compiler.Error as CE (CompileError (..))
import Compiler.Lexer (lex)
import Compiler.Lexer.Types (Token (..))
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseFactor)
import Compiler.Parser.Errors (unexpectedEOF)
import Compiler.Parser.Types (BinaryOperator (..), Expr (..), Factor (..), UnaryOperator (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Compiler.Types (Literal (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (lex)

type Test = UnitTest Factor

unaryComplement :: Test
unaryComplement =
  UnitTest "unary complement" "~42" (lex >=> parse parseFactor) $ Right $ Unary Complement (Lit (LiteralInt 42))

unaryNegation :: Test
unaryNegation =
  UnitTest "unary negation" "-42" (lex >=> parse parseFactor) $ Right $ Unary Negate (Lit (LiteralInt 42))

parenthesizedLiteralInt :: Test
parenthesizedLiteralInt =
  UnitTest "parenthesized literal int" "(42)" (lex >=> parse parseFactor) $ Right $ Expr (Factor (Lit (LiteralInt 42)))

nestedParenthesizedLiteralInt :: Test
nestedParenthesizedLiteralInt =
  UnitTest "nested parenthesized literal int" "(((42)))" (lex >=> parse parseFactor) $ Right $ Expr (Factor (Expr (Factor (Expr (Factor (Lit (LiteralInt 42)))))))

nestedParenthesizedUnaryOperators :: Test
nestedParenthesizedUnaryOperators =
  UnitTest "nested parenthesized unary operators" "-(~(42))" (lex >=> parse parseFactor) $
    Right $
      Unary Negate (Expr (Factor (Unary Complement (Expr (Factor (Lit (LiteralInt 42)))))))

unaryOnParenthesizedExpr :: Test
unaryOnParenthesizedExpr =
  UnitTest "unary on parenthesized expression" "~(7 * 6)" (lex >=> parse parseFactor) $
    Right $
      Unary Complement (Expr (Binary Mul (Factor (Lit (LiteralInt 7))) (Factor (Lit (LiteralInt 6)))))

doubleUnary :: Test
doubleUnary =
  UnitTest "double unary" "~-42" (lex >=> parse parseFactor) $
    Right $
      Unary Complement (Unary Negate (Lit (LiteralInt 42)))

incorrectlyParenthesizedLiteralInt :: Test
incorrectlyParenthesizedLiteralInt =
  UnitTest "incorrectly parenthesized literal int (missing right parenthesis)" "(42" (lex >=> parse parseFactor) $
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

    forM_ unaryFactorTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
