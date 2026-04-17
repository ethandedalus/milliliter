module Compiler.Test.Parser.ParseFactorSpec where

import Compiler.AST
import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseFactor)
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ParsedExpr

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
  result = Lit 42

nestedParenthesized :: Test
nestedParenthesized = UnitTest "nested parenthesized literal int" "(((42)))" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Lit 42

nestedParenthesizedUnaryOperators :: Test
nestedParenthesizedUnaryOperators = UnitTest "nested parenthesized unary operators" "-(~(42))" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Negate (Unary Complement (Lit 42))

unaryOnParenthesizedExpr :: Test
unaryOnParenthesizedExpr = UnitTest "unary on parenthesized expression" "~(7 * 6)" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Complement (Binary Mul (Lit 7) (Lit 6))

doubleUnary :: Test
doubleUnary = UnitTest "double unary" "~-42" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFactor
  result = Unary Complement (Unary Negate (Lit 42))
