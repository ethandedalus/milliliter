module Compiler.Test.Parser.ParseFuncSpec where

import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseFunc)
import Compiler.Parser.Types (Expr (..), Factor (..), Func (..), Stmt (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Compiler.Types (Literal (..))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (lex)

type Test = UnitTest Func

simpleMain :: Test
simpleMain = UnitTest "simple main" "int main(void) { return 0; }" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFunc
  result = Func "main" (Return (Factor $ Lit (LiteralInt 0)))

spec :: Spec
spec = do
  describe "parse functions" $ do
    let parseFuncTestCases = [simpleMain]

    forM_ parseFuncTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
