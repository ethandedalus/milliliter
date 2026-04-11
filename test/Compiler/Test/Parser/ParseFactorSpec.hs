module Compiler.Test.Parser.ParseFactorSpec where

import qualified Compiler.Error as CE (CompileError (..))
import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Lexer.Types (Token (..), tokenName)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseFactor)
import Compiler.Parser.Types (BinaryOperator (..), Expr (..), Factor (..), ParseError (..), UnaryOperator (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Factor

spec :: Spec
spec = describe "unary factor expressions" $ do
  let unaryFactorTestCases =
        [ unaryComplement
        , unaryNegation
        , parenthesized
        , nestedParenthesized
        , nestedParenthesizedUnaryOperators
        , unaryOnParenthesizedExpr
        , doubleUnary
        , incorrectlyParenthesized
        ]

  forM_ unaryFactorTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
    run input `shouldBe` expectedResult

unaryComplement :: Test
unaryComplement = UnitTest "unary complement" "~42" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Complement (Lit 42)

unaryNegation :: Test
unaryNegation = UnitTest "unary negation" "-42" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Negate (Lit 42)

parenthesized :: Test
parenthesized = UnitTest "parenthesized literal int" "(42)" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Expr (Factor (Lit 42))

nestedParenthesized :: Test
nestedParenthesized = UnitTest "nested parenthesized literal int" "(((42)))" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Expr (Factor (Expr (Factor (Expr (Factor (Lit 42))))))

nestedParenthesizedUnaryOperators :: Test
nestedParenthesizedUnaryOperators = UnitTest "nested parenthesized unary operators" "-(~(42))" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Negate (Expr (Factor (Unary Complement (Expr (Factor (Lit 42))))))

unaryOnParenthesizedExpr :: Test
unaryOnParenthesizedExpr = UnitTest "unary on parenthesized expression" "~(7 * 6)" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Complement (Expr (Binary Mul (Factor (Lit 7)) (Factor (Lit 6))))

doubleUnary :: Test
doubleUnary = UnitTest "double unary" "~-42" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Complement (Unary Negate (Lit 42))

incorrectlyParenthesized :: Test
incorrectlyParenthesized = UnitTest "incorrectly parenthesized literal int (missing right parenthesis)" "(42" compile $ Left result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = CE.ParseError (UnexpectedEOF $ "expected " ++ tokenName TRParen)
