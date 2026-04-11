module Compiler.Test.Parser.ParseStmtSpec where

import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseStmt)
import Compiler.Parser.Types (BinaryOperator (..), Expr (..), Factor (..), Stmt (..), UnaryOperator (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Stmt

simpleReturnStmt :: Test
simpleReturnStmt = UnitTest "simple return stmt" "return 0;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = Return (Factor (Lit 0))

parenthesizedReturnExpr :: Test
parenthesizedReturnExpr = UnitTest "parenthesized return expr" "return ~(-(42));" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = Return (Factor $ Unary Complement (Expr (Factor $ Unary Negate (Expr (Factor (Lit 42))))))

binaryReturnExpr :: Test
binaryReturnExpr = UnitTest "binary return expr" "return 1 + 1;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseStmt
  result = Return (Binary Add (Factor (Lit 1)) (Factor (Lit 1)))

spec :: Spec
spec = describe "parse statements" $ do
  let parseStmtTestCases =
        [ simpleReturnStmt
        , parenthesizedReturnExpr
        , binaryReturnExpr
        ]

  forM_ parseStmtTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
    run input `shouldBe` expectedResult
