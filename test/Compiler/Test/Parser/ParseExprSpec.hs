{-# LANGUAGE DataKinds #-}

module Compiler.Test.Parser.ParseExprSpec where

import Compiler.AST
import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseExpr)
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ParsedExpr

spec :: Spec
spec = do
  describe "parse literal expressions" $ do
    let testCases = [literalInt]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "parse unary expressions" $ do
    let testCases = [unaryFactor1, unaryFactor2]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "parse binary expressions" $ do
    let testCases = [simpleAddition, arith1, arithWithGrouping1, arithWithGrouping2, bitwise1]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "parse assignment expressions" $ do
    let testCases = [assignment1, assignment2]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

literalInt :: Test
literalInt = UnitTest "literal int" "42" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Lit 42

unaryFactor1 :: Test
unaryFactor1 = UnitTest "unary factor expression" "~42" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Unary Complement (Lit 42)

unaryFactor2 :: Test
unaryFactor2 = UnitTest "unary factor expression" "~(42)" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Unary Complement (Lit 42)

simpleAddition :: Test
simpleAddition = UnitTest "simple 2 term arithmetic" "21 + 21" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Binary Add (Lit 21) (Lit 21)

arith1 :: Test
arith1 =
  UnitTest "simple 3 term arithmetic" "21 + 3 * 7" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result =
    Binary Add (Lit 21) $
      Binary Mul (Lit 3) (Lit 7)

arithWithGrouping1 :: Test
arithWithGrouping1 =
  UnitTest "arithmetic with grouping (1)" "10 * 3 / (7 + 11)" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result =
    Binary
      Div
      (Binary Mul (Lit 10) (Lit 3))
      (Binary Add (Lit 7) (Lit 11))

arithWithGrouping2 :: Test
arithWithGrouping2 = UnitTest "arithmetic with grouping (2)" "10 + 5 * (2 + 5) - 3" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result =
    Binary
      Sub
      ( Binary
          Add
          (Lit 10)
          (Binary Mul (Lit 5) (Binary Add (Lit 2) (Lit 5)))
      )
      (Lit 3)

bitwise1 :: Test
bitwise1 = UnitTest "bitwise precedence" "1 << 2 & 3 | 4 >> 5 ^ 6" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result =
    Binary
      BitOr
      (Binary BitAnd (Binary LeftShift (Lit 1) (Lit 2)) (Lit 3))
      (Binary Xor (Binary RightShift (Lit 4) (Lit 5)) (Lit 6))

assignment1 :: Test
assignment1 = UnitTest "simple assignment" "a = b" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Assign (VarP "a") (VarP "b")

assignment2 :: Test
assignment2 = UnitTest "compound assignment" "a = b = c = d" compile $ pure result
 where
  compile = Lexer.lex >=> parse (parseExpr 0)
  result = Assign (VarP "a") (Assign (VarP "b") (Assign (VarP "c") (VarP "d")))
