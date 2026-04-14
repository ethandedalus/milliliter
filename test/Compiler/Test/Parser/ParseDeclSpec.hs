module Compiler.Test.Parser.ParseDeclSpec where

import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseDecl)
import Compiler.Parser.Types
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Decl

spec :: Spec
spec = do
  describe "parse functions" $ do
    let parseFuncTestCases = [decl1, decl2, decl3]

    forM_ parseFuncTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

decl1 :: Test
decl1 = UnitTest "simple decl (1)" "int a = 5;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseDecl
  result = Decl "a" $ pure (Factor (Lit 5))

decl2 :: Test
decl2 = UnitTest "simple decl (2)" "int b = a;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseDecl
  result = Decl "b" $ pure (Factor (Ident "a"))

decl3 :: Test
decl3 = UnitTest "decl without initializer" "int a;" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseDecl
  result = Decl "a" Nothing
