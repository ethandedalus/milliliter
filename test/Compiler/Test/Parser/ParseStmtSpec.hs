module Compiler.Test.Parser.ParseStmtSpec where

import Compiler.Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseStmt)
import Compiler.Parser.Types (BinaryOperator (..), Expr (..), Factor (..), Stmt (..), UnaryOperator (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Compiler.Types (Literal (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (lex)

type Test = UnitTest Stmt

simpleReturnStmt :: Test
simpleReturnStmt =
  UnitTest
    "simple return stmt"
    "return 0;"
    (lex >=> parse parseStmt)
    $ Right
    $ Return (Factor (Lit (LiteralInt 0)))

parenthesizedReturnExpr :: Test
parenthesizedReturnExpr =
  UnitTest "parenthesized return expr" "return ~(-(42));" (lex >=> parse parseStmt) $
    Right $
      Return (Factor $ Unary Complement (Expr (Factor $ Unary Negate (Expr (Factor (Lit (LiteralInt 42)))))))

binaryReturnExpr :: Test
binaryReturnExpr =
  UnitTest "binary return expr" "return 1 + 1;" (lex >=> parse parseStmt) $
    Right $
      Return (Binary Add (Factor (Lit (LiteralInt 1))) (Factor (Lit (LiteralInt 1))))

spec :: Spec
spec = do
  describe "parse statements" $ do
    let parseStmtTestCases =
          [ simpleReturnStmt
          , parenthesizedReturnExpr
          , binaryReturnExpr
          ]

    forM_ parseStmtTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
