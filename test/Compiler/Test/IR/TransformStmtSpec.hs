module Compiler.Test.IR.TransformStmtSpec where

import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformStmt)
import Compiler.IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseStmt)
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest [Instruction]

spec :: Spec
spec = describe "lower return statements" $ do
  let testCases = [transformReturn1, tranformReturn2]

  forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
    run input `shouldBe` expectedResult

transformReturn1 :: Test
transformReturn1 = UnitTest "simple return" "return 42;" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseStmt >=> IR.transform IR.transformStmt
  result = [Return (Lit 42)]

tranformReturn2 :: Test
tranformReturn2 = UnitTest "return nested unary expr" "return -~-42;" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseStmt >=> IR.transform IR.transformStmt
  result =
    [ Unary Negate (Lit 42) (Var "tmp.0")
    , Unary Complement (Var "tmp.0") (Var "tmp.1")
    , Unary Negate (Var "tmp.1") (Var "tmp.2")
    , Return (Var "tmp.2")
    ]
