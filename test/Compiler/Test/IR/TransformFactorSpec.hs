module Compiler.Test.IR.TransformFactorSpec where

import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformFactor)
import Compiler.IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseFactor)
import Compiler.Test.Shared.UnitTest (UnitTest (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (lex)

type Test = UnitTest ([Instruction], Val)

spec :: Spec
spec = describe "transform factor" $ do
  let testCases = [transformLiteral, transformUnaryOperation1, transformUnaryOperation2, transformUnaryOperation3]

  forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
    run input `shouldBe` expectedResult

transformLiteral :: Test
transformLiteral = UnitTest "literal int" "42" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseFactor >=> IR.transform IR.transformFactor
  result = ([], Lit 42)

transformUnaryOperation1 :: Test
transformUnaryOperation1 = UnitTest "unary operation" "~42" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseFactor >=> IR.transform IR.transformFactor
  result = ([Unary Complement (Lit 42) (Var "tmp.0")], Var "tmp.0")

transformUnaryOperation2 :: Test
transformUnaryOperation2 = UnitTest "unary operation (parenthesized)" "~(42)" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseFactor >=> IR.transform IR.transformFactor
  result = ([Unary Complement (Lit 42) (Var "tmp.0")], Var "tmp.0")

transformUnaryOperation3 :: Test
transformUnaryOperation3 = UnitTest "multiple unary operations" "~(-42)" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseFactor >=> IR.transform IR.transformFactor
  result =
    (
      [ Unary Negate (Lit 42) (Var "tmp.0")
      , Unary Complement (Var "tmp.0") (Var "tmp.1")
      ]
    , Var "tmp.1"
    )
