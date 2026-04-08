module Parser.ParseFuncSpec where

import qualified CompileError as CE (CompileError (..))
import Compiler.Types (Literal (..))
import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)
import Lexer (runLexer)
import Lexer.Types (mkSourceLoc)
import Parser.Combinators (parseFunc)
import Parser.Types (Expr (..), Factor (..), Func (..), Stmt (..))
import Shared.UnitTest (UnitTest (UnitTest))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Func

simpleMain :: Test
simpleMain =
  UnitTest "simple main" "int main(void) { return 0; }" $
    Right $
      Func "main" (Return (Factor $ Lit (LiteralInt 0)))

spec :: Spec
spec = do
  describe "parse functions" $ do
    let parseFuncTestCases = [simpleMain]

    forM_ parseFuncTestCases $ \(UnitTest caseName input expectedResult) -> it ("case: " ++ caseName) $ do
      expectedResult `shouldBe` do
        tokens <- first CE.LexError $ evalStateT runLexer (input, mkSourceLoc 1 1)
        first CE.ParseError $ evalStateT parseFunc tokens
