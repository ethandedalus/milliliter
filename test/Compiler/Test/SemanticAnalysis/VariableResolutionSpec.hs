module Compiler.Test.SemanticAnalysis.VariableResolutionSpec where

import Compiler.AST
import Compiler.Error (CompileError)
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P
import qualified Compiler.SemanticAnalysis as S (analyzeWithState)
import qualified Compiler.SemanticAnalysis.Types as S
import qualified Compiler.SemanticAnalysis.VariableResolution as S
import Compiler.Test.Shared.UnitTest (Binding (..), UnitTest (UnitTest), prepareEnv)
import Control.Monad (forM_, (>=>))
import Data.Map ((!))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest ResolvedExpr

compile ::
  S.ScopeContext ->
  S.SemanticAnalysisState ->
  String ->
  Either CompileError ResolvedExpr
compile ctx sas = Lexer.lex >=> P.parse P.parseFactor >=> analyze
 where
  analyze = S.analyzeWithState ctx sas S.resolveExpr

spec :: Spec
spec = do
  describe "resolve factor" $ do
    let testCases = [factor1, factor2, factor3]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

factor1 :: Test
factor1 = UnitTest "literal" "1" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result = Lit 1

factor2 :: Test
factor2 = UnitTest "unary (1)" "~5" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result = Unary Complement (Lit 5)

factor3 :: Test
factor3 = UnitTest "unary (2)" "~a" (compile ctx sas) $ pure result
 where
  (ctx, sas, names) = prepareEnv [Binding 0 "a"]
  result = Unary Complement (Var 0 (names ! "a"))
