{-# LANGUAGE TupleSections #-}

module Parser.ParseExprTestCases (
  UnitTest (..),
  literalExpressionsTestCases,
  unaryExpressionsTestCases,
  binaryExpressionsTestCases,
)
where

import Compiler.Types (Literal (..))
import Lexer.Types (Span, Token (..), mkNilSpan)
import Parser.Errors (ParseError)
import Parser.Types (BinaryOperator (..), Expr (..), Factor (..), UnaryOperator (..))

data UnitTest = UnitTest
  { name :: String
  , tokenStream :: [(Token, Span)]
  , expected :: Either ParseError Expr
  }

mkUnitTest :: String -> [Token] -> Either ParseError Expr -> UnitTest
mkUnitTest n ts = UnitTest n ((,mkNilSpan) <$> ts)

unitTestLiteralString
  , unitTestLiteralInt
  , unitTestLiteralFloat
  , unitTestLiteralChar
  , unitTestLiteralBool ::
    UnitTest
unitTestLiteralString = mkUnitTest "literal string" [TLiteral (LiteralString "hello, world")] $ Right $ Factor (Lit (LiteralString "hello, world"))
unitTestLiteralInt = mkUnitTest "literal int" [TLiteral (LiteralInt 42)] $ Right $ Factor (Lit (LiteralInt 42))
unitTestLiteralFloat = mkUnitTest "literal float" [TLiteral (LiteralFloat 42.0)] $ Right $ Factor (Lit (LiteralFloat 42.0))
unitTestLiteralChar = mkUnitTest "literal char" [TLiteral (LiteralChar 'x')] $ Right $ Factor (Lit (LiteralChar 'x'))
unitTestLiteralBool = mkUnitTest "literal bool" [TLiteral (LiteralBool True)] $ Right $ Factor (Lit (LiteralBool True))

literalExpressionsTestCases :: [UnitTest]
literalExpressionsTestCases =
  [ unitTestLiteralString
  , unitTestLiteralInt
  , unitTestLiteralFloat
  , unitTestLiteralChar
  , unitTestLiteralBool
  ]

unitTestUnaryFactor :: UnitTest
unitTestUnaryFactor =
  mkUnitTest
    "unary expression"
    [TNot, TLParen, TLiteral (LiteralInt 42), TRParen]
    $ Right (Factor (Unary Complement (Expr (Factor (Lit (LiteralInt 42))))))

unaryExpressionsTestCases :: [UnitTest]
unaryExpressionsTestCases =
  [unitTestUnaryFactor]

unitTestBinaryExpression1 ::
  UnitTest
unitTestBinaryExpression1 =
  mkUnitTest "binary expression {21 + 21}" [TLiteral (LiteralInt 21), TPlus, TLiteral (LiteralInt 21)] $
    Right $
      Binary Add (Factor (Lit (LiteralInt 21))) (Factor (Lit (LiteralInt 21)))

unitTestBinaryExpression2 :: UnitTest
unitTestBinaryExpression2 =
  mkUnitTest "binary expression {21 + 3 * 7}" [TLiteral (LiteralInt 21), TPlus, TLiteral (LiteralInt 3), TStar, TLiteral (LiteralInt 7)] $
    Right $
      Binary Add (Factor (Lit (LiteralInt 21))) $
        Binary Mul (Factor (Lit (LiteralInt 3))) (Factor (Lit (LiteralInt 7)))

unitTestBinaryExpression3 :: UnitTest
unitTestBinaryExpression3 =
  mkUnitTest
    "binary expression {10 + 3 * 7 + 11}"
    [TLiteral (LiteralInt 10), TPlus, TLiteral (LiteralInt 3), TStar, TLiteral (LiteralInt 7), TPlus, TLiteral (LiteralInt 11)]
    $ Right
    $ Binary
      Add
      ( Binary Add (Factor (Lit (LiteralInt 10))) $
          Binary Mul (Factor (Lit (LiteralInt 3))) (Factor (Lit (LiteralInt 7)))
      )
    $ Factor (Lit (LiteralInt 11))

unitTestBinaryExpression4 :: UnitTest
unitTestBinaryExpression4 =
  mkUnitTest
    "binary expression {10 + 5 * (2 + 5) - 3}"
    [ TLiteral (LiteralInt 10)
    , TPlus
    , TLiteral (LiteralInt 5)
    , TStar
    , TLParen
    , TLiteral (LiteralInt 2)
    , TPlus
    , TLiteral (LiteralInt 5)
    , TRParen
    , TMinus
    , TLiteral (LiteralInt 3)
    ]
    $ Right
      ( Binary
          Sub -- (expr) - 3
          ( Binary
              Add -- (10 + (expr))
              (Factor (Lit (LiteralInt 10)))
              ( Binary
                  Mul -- (5 * (expr))
                  (Factor (Lit (LiteralInt 5)))
                  ( Factor
                      ( Expr
                          ( Binary
                              Add -- 2 * 5
                              (Factor (Lit (LiteralInt 2)))
                              (Factor (Lit (LiteralInt 5)))
                          )
                      )
                  )
              )
          )
          (Factor (Lit (LiteralInt 3)))
      )

binaryExpressionsTestCases :: [UnitTest]
binaryExpressionsTestCases =
  [ unitTestBinaryExpression1
  , unitTestBinaryExpression2
  , unitTestBinaryExpression3
  , unitTestBinaryExpression4
  ]
