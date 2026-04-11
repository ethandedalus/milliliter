module Compiler.Test.IR.LowerStmtSpec where

import qualified Compiler.IR as IR (lower)
import qualified Compiler.IR.Lower as IR (lowerStmt)
import Compiler.IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseStmt)
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Compiler.Types (Literal (LiteralInt))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest [Instruction]

simpleReturn :: Test
simpleReturn = UnitTest "simple return" "return 42;" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseStmt >=> IR.lower IR.lowerStmt
  result = [Return (Lit (LiteralInt 42))]

returnNestedUnaryExpr :: Test
returnNestedUnaryExpr = UnitTest "return nested unary expr" "return -~-42;" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseStmt >=> IR.lower IR.lowerStmt
  result =
    [ Unary Negate (Lit $ LiteralInt 42) (Var "tmp.0")
    , Unary Complement (Var "tmp.0") (Var "tmp.1")
    , Unary Negate (Var "tmp.1") (Var "tmp.2")
    , Return (Var "tmp.2")
    ]

spec :: Spec
spec = do
  describe "lower return statements" $ do
    let lowerStmtTestCases = [simpleReturn, returnNestedUnaryExpr]

    forM_ lowerStmtTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
