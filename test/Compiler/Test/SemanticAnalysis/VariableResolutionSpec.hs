module Compiler.Test.SemanticAnalysis.VariableResolutionSpec where

import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P
import qualified Compiler.Parser.Types as P
import qualified Compiler.SemanticAnalysis as S (analyze, analyzeWithState)
import Compiler.SemanticAnalysis.Types
import qualified Compiler.SemanticAnalysis.VariableResolution as S
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import qualified Data.Map as Map
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "resolve factor" $ do
    let testCases = [factor1, factor2, factor3]

    forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

factor1 :: UnitTest P.Factor
factor1 = UnitTest "literal" "1" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseFactor >=> S.analyze S.resolveFactor
  result = P.Lit 1

factor2 :: UnitTest P.Factor
factor2 = UnitTest "unary (1)" "~5" compile $ pure result
 where
  compile = Lexer.lex >=> P.parse P.parseFactor >=> S.analyze S.resolveFactor
  result = P.Unary P.Complement (P.Lit 5)

factor3 :: UnitTest P.Factor
factor3 = UnitTest "unary (2)" "~a" compile $ pure result
 where
  compile =
    Lexer.lex
      >=> P.parse P.parseFactor
      >=> S.analyzeWithState
        SemanticAnalysisState{_varSeq = 1, _varMap = Map.fromList [("a", "a$0")]}
        S.resolveFactor
  result = P.Unary P.Complement (P.Ident "a$0")
