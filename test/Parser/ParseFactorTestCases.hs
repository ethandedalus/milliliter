{-# LANGUAGE TupleSections #-}

module Parser.ParseFactorTestCases (unaryFactorTestCases, UnitTest (..)) where

import Compiler.Types (Literal (..))
import Lexer.Types (Span, Token (..), mkNilSpan)
import Parser.Errors (ParseError (..), unexpectedEOF)
import Parser.Types (BinaryOperator (..), Expr (..), Factor (..), UnaryOperator (..))

data UnitTest = UnitTest
  { name :: String
  , tokenStream :: [(Token, Span)]
  , expected :: Either ParseError Factor
  }

mkUnitTest :: String -> [Token] -> Either ParseError Factor -> UnitTest
mkUnitTest n ts = UnitTest n ((,mkNilSpan) <$> ts)

unitTestUnaryComplement :: UnitTest
unitTestUnaryComplement = mkUnitTest "unary not" [TNot, TLiteral (LiteralInt 42)] $ Right $ Unary Complement (Lit (LiteralInt 42))

unitTestUnaryNegation :: UnitTest
unitTestUnaryNegation = mkUnitTest "unary negation" [TMinus, TLiteral (LiteralInt 42)] $ Right $ Unary Negate (Lit (LiteralInt 42))

unitTestParenthesizedLiteralInt :: UnitTest
unitTestParenthesizedLiteralInt =
  mkUnitTest "parenthesized literal int" [TLParen, TLiteral (LiteralInt 42), TRParen] $
    Right $
      Expr (Factor (Lit (LiteralInt 42)))

unitTestNestedParenthesizedLiteralInt :: UnitTest
unitTestNestedParenthesizedLiteralInt =
  mkUnitTest "nested (3x) parenthesized literal int" [TLParen, TLParen, TLParen, TLiteral (LiteralInt 42), TRParen, TRParen, TRParen] $
    Right $
      Expr (Factor (Expr (Factor (Expr (Factor (Lit (LiteralInt 42)))))))

unitTestParenthesizedUnaryOperators :: UnitTest
unitTestParenthesizedUnaryOperators =
  mkUnitTest "nested parenthesized unary operators" [TMinus, TLParen, TNot, TLParen, TLiteral (LiteralInt 42), TRParen, TRParen] $
    Right $
      Unary Negate (Expr (Factor (Unary Complement (Expr (Factor (Lit (LiteralInt 42)))))))

unitTestIncorrectlyParenthesizedLiteralInt :: UnitTest
unitTestIncorrectlyParenthesizedLiteralInt =
  mkUnitTest
    "incorrectly parenthesized literal int (missing right parenthesis)"
    [TLParen, TLiteral (LiteralInt 42)]
    $ Left
    $ unexpectedEOF (show TRParen)

unitTest1 :: UnitTest
unitTest1 =
  mkUnitTest
    "binary expression ~(1 + 1)"
    [TNot, TLParen, TLiteral (LiteralInt 1), TPlus, TLiteral (LiteralInt 1), TRParen]
    $ Right
    $ Unary Complement (Expr (Binary Add (Factor (Lit (LiteralInt 1))) (Factor (Lit (LiteralInt 1)))))

unaryFactorTestCases :: [UnitTest]
unaryFactorTestCases =
  [ unitTestUnaryComplement
  , unitTestUnaryNegation
  , unitTestParenthesizedLiteralInt
  , unitTestNestedParenthesizedLiteralInt
  , unitTestParenthesizedUnaryOperators
  , unitTestIncorrectlyParenthesizedLiteralInt
  , unitTest1
  ]
