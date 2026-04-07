{-# LANGUAGE TupleSections #-}

module Parser.ParseStmtTestCases where

import Compiler.Types (Literal (..))
import Lexer.Types (Span, Token (..), mkNilSpan)
import Parser.Errors (ParseError (..))
import Parser.Types (BinaryOperator (..), Expr (..), Factor (..), Stmt (..), UnaryOperator (..))

data UnitTest = UnitTest
  { name :: String
  , input :: [(Token, Span)]
  , expected :: Either ParseError Stmt
  }

mkUnitTest :: String -> [Token] -> Either ParseError Stmt -> UnitTest
mkUnitTest n ts = UnitTest n ((,mkNilSpan) <$> ts)

unitTestReturnStmt :: UnitTest
unitTestReturnStmt =
  mkUnitTest "return stmt" [TReturn, TLiteral (LiteralInt 0), TSemicolon] $ Right $ Return (Factor (Lit (LiteralInt 0)))

unitTestReturnStmtParenthesized :: UnitTest
unitTestReturnStmtParenthesized =
  mkUnitTest "return stmt (parenthesized)" [TReturn, TNot, TLParen, TMinus, TLParen, TLiteral (LiteralInt 42), TRParen, TRParen, TSemicolon] $
    Right $
      Return (Factor $ Unary Complement (Expr (Factor $ Unary Negate (Expr (Factor (Lit (LiteralInt 42)))))))

unitTestReturnStmtExpr :: UnitTest
unitTestReturnStmtExpr =
  mkUnitTest "return stmt (expr)" [TReturn, TNot, TLParen, TLiteral (LiteralInt 1), TPlus, TLiteral (LiteralInt 1), TRParen, TSemicolon] $
    Right $
      Return (Factor (Unary Complement (Expr (Binary Add (Factor (Lit (LiteralInt 1))) (Factor (Lit (LiteralInt 1)))))))

parseStmtTestCases :: [UnitTest]
parseStmtTestCases =
  [ unitTestReturnStmt
  , unitTestReturnStmtParenthesized
  , unitTestReturnStmtExpr
  ]

-- describe "parse statements" $ do
--   let cases =
--         [ ( "return statement",
--             fmap
--               (,nilSpan)
--               [TReturn, TLiteral (CT.LiteralInt 0), TSemicolon],
--             Right $ Return (Lit (CT.LiteralInt 0))
--           )
--         ]
--
--   forM_ cases $ \(caseName, input, expected) ->
--     it ("case: " ++ caseName) $ evalStateT parseStmt input `shouldBe` expected
--
-- describe "parse functions" $ do
--   let cases =
--         [ ( "function [1]",
--             fmap
--               (,nilSpan)
--               [ TInt,
--                 TIdent "main",
--                 TLParen,
--                 TVoid,
--                 TRParen,
--                 TLBrace,
--                 TReturn,
--                 TLiteral (CT.LiteralInt 0),
--                 TSemicolon,
--                 TRBrace
--               ],
--             Right $ Func "main" (Return (Lit (CT.LiteralInt 0)))
--           )
--         ]
--
--   forM_ cases $ \(caseName, input, expected) ->
--     it ("case: " ++ caseName) $ evalStateT parseFunc input `shouldBe` expected
