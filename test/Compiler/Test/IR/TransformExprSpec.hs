module Compiler.Test.IR.TransformExprSpec where

import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformExpr)
import Compiler.IR.Types (BinaryOperator (..), Instruction (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseExpr)
import Compiler.Test.Shared.UnitTest (UnitTest (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ([Instruction], Val)

spec :: Spec
spec = describe "transform binary expressions" $ do
  let testCases =
        [ transformBinaryAddition1
        , transformBinaryAddition2
        , transformArithmetic1
        ]

  forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
    run input `shouldBe` expectedResult

transformBinaryAddition1 :: Test
transformBinaryAddition1 = UnitTest "simple binary addition" "1 + 1" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse (P.parseExpr 0) >=> IR.transform IR.transformExpr
  result = ([Binary Add (Lit 1) (Lit 1) (Var "tmp.0")], Var "tmp.0")

transformBinaryAddition2 :: Test
transformBinaryAddition2 = UnitTest "parenthesized binary addition" "(1 + 1)" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse (P.parseExpr 0) >=> IR.transform IR.transformExpr
  result = ([Binary Add (Lit 1) (Lit 1) (Var "tmp.0")], Var "tmp.0")

transformArithmetic1 :: Test
transformArithmetic1 = UnitTest "general arithmetic expression (1)" "1 + 2 * (3 - 4) / 5" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse (P.parseExpr 0) >=> IR.transform IR.transformExpr
  result =
    (
      [ Binary Sub (Lit 3) (Lit 4) (Var "tmp.0")
      , Binary Mul (Lit 2) (Var "tmp.0") (Var "tmp.1")
      , Binary Div (Var "tmp.1") (Lit 5) (Var "tmp.2")
      , Binary Add (Lit 1) (Var "tmp.2") (Var "tmp.3")
      ]
    , Var "tmp.3"
    )
