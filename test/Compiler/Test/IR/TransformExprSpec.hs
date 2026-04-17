module Compiler.Test.IR.TransformExprSpec where

import Compiler.Error (CompileError)
import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformExpr)
import Compiler.IR.Types (BinaryOperator (..), Instruction (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseExpr)
import qualified Compiler.SemanticAnalysis as S
import qualified Compiler.SemanticAnalysis.Types as S
import qualified Compiler.SemanticAnalysis.VariableResolution as S
import Compiler.Test.Shared.UnitTest (Binding (..), UnitTest (..), prepareEnv)
import Control.Monad (forM_, (>=>))
import Data.Map ((!))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ([Instruction], Val)

compile ::
  S.ScopeContext ->
  S.SemanticAnalysisState ->
  String ->
  Either CompileError ([Instruction], Val)
compile ctx sas = Lexer.lex >=> P.parse (P.parseExpr 0) >=> analyze >=> IR.transform IR.transformExpr
 where
  analyze = S.analyzeWithState ctx sas S.resolveExpr

spec :: Spec
spec = do
  describe "transform binary expressions" $ do
    let testCases =
          [ transformBinaryAddition1
          , transformBinaryAddition2
          , transformArithmetic1
          ]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

  describe "transform compound expressions" $ do
    let testCases = [transformCompound1]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

transformBinaryAddition1 :: Test
transformBinaryAddition1 = UnitTest "simple binary addition" "1 + 1" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result = ([Binary Add (Lit 1) (Lit 1) (Var "tmp.0")], Var "tmp.0")

transformBinaryAddition2 :: Test
transformBinaryAddition2 = UnitTest "parenthesized binary addition" "(1 + 1)" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result = ([Binary Add (Lit 1) (Lit 1) (Var "tmp.0")], Var "tmp.0")

transformArithmetic1 :: Test
transformArithmetic1 = UnitTest "general arithmetic expression (1)" "1 + 2 * (3 - 4) / 5" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    (
      [ Binary Sub (Lit 3) (Lit 4) (Var "tmp.0")
      , Binary Mul (Lit 2) (Var "tmp.0") (Var "tmp.1")
      , Binary Div (Var "tmp.1") (Lit 5) (Var "tmp.2")
      , Binary Add (Lit 1) (Var "tmp.2") (Var "tmp.3")
      ]
    , Var "tmp.3"
    )

transformCompound1 :: Test
transformCompound1 = UnitTest "compound assignment" "a += b" (compile ctx sas) $ pure result
 where
  (ctx, sas, names) = prepareEnv [Binding 0 "a", Binding 0 "b"]
  a = names ! "a"
  b = names ! "b"

  result = ([Binary Add (Var a) (Var b) (Var a)], Var a)
