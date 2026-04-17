module Compiler.Test.IR.TransformStmtSpec where

import Compiler.Error (CompileError)
import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformStmt)
import Compiler.IR.Types (Instruction (..), UnaryOperator (..), Val (..))
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P (parseStmt)
import qualified Compiler.SemanticAnalysis as S
import qualified Compiler.SemanticAnalysis.LabelValidation as S
import qualified Compiler.SemanticAnalysis.Types as S
import qualified Compiler.SemanticAnalysis.VariableResolution as S
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest), prepareEnv)
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest [Instruction]

compile ::
  S.ScopeContext ->
  S.SemanticAnalysisState ->
  String ->
  Either CompileError [Instruction]
compile ctx sas = Lexer.lex >=> P.parse P.parseStmt >=> analyze >=> IR.transform IR.transformStmt
 where
  analyze = S.analyzeWithState ctx sas (S.resolveStmt >=> S.validateStmt)

spec :: Spec
spec = describe "lower return statements" $ do
  let testCases = [transformReturn1, tranformReturn2]

  forM_ testCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
    run input `shouldBe` expectedResult

transformReturn1 :: Test
transformReturn1 = UnitTest "simple return" "return 42;" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result = [Return (Lit 42)]

tranformReturn2 :: Test
tranformReturn2 = UnitTest "return nested unary expr" "return -~-42;" (compile ctx sas) $ pure result
 where
  (ctx, sas, _) = prepareEnv []
  result =
    [ Unary Negate (Lit 42) (Var "tmp.0")
    , Unary Complement (Var "tmp.0") (Var "tmp.1")
    , Unary Negate (Var "tmp.1") (Var "tmp.2")
    , Return (Var "tmp.2")
    ]
