module Compiler.Test.Parser.ParseStmtSpec where

import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseStmt)
import Compiler.Parser.Types (BinaryOperator (..), Expr (..), Factor (..), Stmt (..), UnaryOperator (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Stmt

spec :: Spec
spec = do
  describe "parse return statements" $ do
    let testCases =
          [ returnSimple
          , returnParenthesizedExpr
          , returnBinaryExpr
          ]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "parse empty statements" $ do
    let testCases = [emptyStmt]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "parse expression statements" $ do
    let testCases = [exprStmt1, exprStmt2]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

returnSimple :: Test
returnSimple = UnitTest "simple return stmt" "return 0;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = Return (Factor (Lit 0))

returnParenthesizedExpr :: Test
returnParenthesizedExpr = UnitTest "parenthesized return expr" "return ~(-(42));" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = Return (Factor $ Unary Complement (Expr (Factor $ Unary Negate (Expr (Factor (Lit 42))))))

returnBinaryExpr :: Test
returnBinaryExpr = UnitTest "binary return expr" "return 1 + 1;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = Return (Binary Add (Factor (Lit 1)) (Factor (Lit 1)))

emptyStmt :: Test
emptyStmt = UnitTest "empty stmt" ";" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = Null

exprStmt1 :: Test
exprStmt1 = UnitTest "expression statement (1)" "a = 5;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = ExprS (Assign (Factor (Ident "a")) (Factor (Lit 5)))

exprStmt2 :: Test
exprStmt2 = UnitTest "expression statement (2)" "~(a = 1);" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = ExprS (Factor (Unary Complement (Expr (Assign (Factor (Ident "a")) (Factor (Lit 1))))))
