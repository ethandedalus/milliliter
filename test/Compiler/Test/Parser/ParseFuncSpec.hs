module Compiler.Test.Parser.ParseFuncSpec where

import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Parser (parse)
import Compiler.Parser.Combinators (parseFunc)
import Compiler.Parser.Types
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_, (>=>))
import Test.Hspec (Spec, describe, it, shouldBe)

type Test = UnitTest Func

spec :: Spec
spec = do
  describe "parse functions" $ do
    let parseFuncTestCases = [func1, func2, func3]

    forM_ parseFuncTestCases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult

func1 :: Test
func1 = UnitTest "simple main" "int main(void) { return 0; }" compile $ pure result
 where
  compile = Lexer.lex >=> parse parseFunc
  result = Func "main" [S $ Return (Factor $ Lit 0)]

func2 :: Test
func2 = UnitTest "function with assignments" program compile $ pure result
 where
  program = "int main(void) { int a = 5; a = a + 5; return a; }"
  compile = Lexer.lex >=> parse parseFunc
  result =
    Func
      { name = "main"
      , body =
          [ D (Decl "a" (Just (Factor (Lit 5))))
          , S (ExprS (Assign (Factor (Ident "a")) (Binary Add (Factor (Ident "a")) (Factor (Lit 5)))))
          , S (Return (Factor (Ident "a")))
          ]
      }

func3 :: Test
func3 = UnitTest "using the return value of an assignment" program compile $ pure result
 where
  program = "int main(void) { int a = 5; int b = 2 + (a = 4); return b; }"
  compile = Lexer.lex >=> parse parseFunc
  result =
    Func
      { name = "main"
      , body =
          [ D (Decl "a" (Just (Factor (Lit 5))))
          , D (Decl "b" (Just (Binary Add (Factor (Lit 2)) (Factor (Expr (Assign (Factor (Ident "a")) (Factor (Lit 4))))))))
          , S (Return (Factor (Ident "b")))
          ]
      }
