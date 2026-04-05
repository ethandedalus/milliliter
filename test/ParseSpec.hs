{-# LANGUAGE TupleSections #-}

module ParseSpec where

import qualified Compiler.Types as CT (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Lexer.Types (Span (..), Token (..), mkSourceLoc, mkSpan)
import Parser.Combinators (parseExpr, parseFunc, parseStmt)
import Parser.Errors (unexpectedEOF)
import Parser.Types (Expr (..), Func (..), Stmt (..), UnaryOperator (..))
import Test.Hspec (Spec, describe, it, shouldBe)

nilSpan :: Span
nilSpan = mkSpan (mkSourceLoc 0 0) (mkSourceLoc 0 0)

spec :: Spec
spec = do
  describe "parse literal expressions" $ do
    let cases =
          [ ( "literal string",
              fmap (,nilSpan) [TLiteral (CT.LiteralString "hello, world")],
              Right $ Lit (CT.LiteralString "hello, world")
            ),
            ( "literal int",
              fmap (,nilSpan) [TLiteral (CT.LiteralInt 5)],
              Right $ Lit (CT.LiteralInt 5)
            ),
            ( "literal float",
              fmap (,nilSpan) [TLiteral (CT.LiteralFloat 5.0)],
              Right $ Lit (CT.LiteralFloat 5.0)
            ),
            ( "literal char",
              fmap (,nilSpan) [TLiteral (CT.LiteralChar 'c')],
              Right $ Lit (CT.LiteralChar 'c')
            ),
            ( "literal bool",
              fmap (,nilSpan) [TLiteral (CT.LiteralBool True)],
              Right $ Lit (CT.LiteralBool True)
            )
          ]

    forM_ cases $ \(caseName, input, expected) ->
      it ("case: " ++ caseName) $ evalStateT parseExpr input `shouldBe` expected

  describe "parse unary expressions" $ do
    let cases =
          [ ( "unary not",
              fmap (,nilSpan) [TNot, TLiteral (CT.LiteralInt 1)],
              Right $ Unary Complement (Lit (CT.LiteralInt 1))
            ),
            ( "unary negation",
              fmap (,nilSpan) [TMinus, TLiteral (CT.LiteralInt 1)],
              Right $ Unary Negate (Lit (CT.LiteralInt 1))
            ),
            ( "parenthesized literal int",
              fmap (,nilSpan) [TLParen, TLiteral (CT.LiteralInt 5), TRParen],
              Right $ Lit (CT.LiteralInt 5)
            ),
            ( "nested (3x) parenthesized literal int",
              fmap (,nilSpan) [TLParen, TLParen, TLParen, TLiteral (CT.LiteralInt 5), TRParen, TRParen, TRParen],
              Right $ Lit (CT.LiteralInt 5)
            ),
            ( "nested parenthesized unary operators",
              fmap (,nilSpan) [TMinus, TLParen, TNot, TLParen, TLiteral (CT.LiteralInt 5), TRParen, TRParen],
              Right $ Unary Negate (Unary Complement $ Lit (CT.LiteralInt 5))
            ),
            ( "incorrectly parenthesized literal int (missing right parenthesis) [1]",
              fmap (,nilSpan) [TLParen, TLiteral (CT.LiteralInt 5)],
              Left $ unexpectedEOF (show TRParen)
            ),
            ( "incorrectly parenthesized literal int (missing right parenthesis) [2]",
              fmap (,nilSpan) [TLParen, TLParen, TLiteral (CT.LiteralInt 5), TRParen],
              Left $ unexpectedEOF (show TRParen)
            )
          ]

    forM_ cases $ \(caseName, input, expected) ->
      it ("case: " ++ caseName) $ evalStateT parseExpr input `shouldBe` expected

  describe "parse statements" $ do
    let cases =
          [ ( "return statement",
              fmap
                (,nilSpan)
                [TReturn, TLiteral (CT.LiteralInt 0), TSemicolon],
              Right $ Return (Lit (CT.LiteralInt 0))
            )
          ]

    forM_ cases $ \(caseName, input, expected) ->
      it ("case: " ++ caseName) $ evalStateT parseStmt input `shouldBe` expected

  describe "parse functions" $ do
    let cases =
          [ ( "function [1]",
              fmap
                (,nilSpan)
                [ TInt,
                  TIdent "main",
                  TLParen,
                  TVoid,
                  TRParen,
                  TLBrace,
                  TReturn,
                  TLiteral (CT.LiteralInt 0),
                  TSemicolon,
                  TRBrace
                ],
              Right $ Func "main" (Return (Lit (CT.LiteralInt 0)))
            )
          ]

    forM_ cases $ \(caseName, input, expected) ->
      it ("case: " ++ caseName) $ evalStateT parseFunc input `shouldBe` expected
