module Compiler.Test.Lexer.LexSpec where

import qualified Compiler.Lexer as Lexer (lex)
import Compiler.Lexer.Types (SourceLoc (..), Span (..), Token (..))
import Compiler.Test.Shared.UnitTest (UnitTest (UnitTest))
import Control.Monad (forM_)
import Data.List (intercalate)
import Test.Hspec (Spec, describe, it, shouldBe)

lexCase1 :: UnitTest [(Token, Span)]
lexCase1 =
  UnitTest "match simple program" "int main(void) { return 0; }" Lexer.lex result
 where
  result =
    pure
      [ (TInt, Span{start = SourceLoc{line = 1, col = 1}, end = SourceLoc{line = 1, col = 4}})
      , (TIdent "main", Span{start = SourceLoc{line = 1, col = 5}, end = SourceLoc{line = 1, col = 9}})
      , (TLParen, Span{start = SourceLoc{line = 1, col = 9}, end = SourceLoc{line = 1, col = 10}})
      , (TVoid, Span{start = SourceLoc{line = 1, col = 10}, end = SourceLoc{line = 1, col = 14}})
      , (TRParen, Span{start = SourceLoc{line = 1, col = 14}, end = SourceLoc{line = 1, col = 15}})
      , (TLBrace, Span{start = SourceLoc{line = 1, col = 16}, end = SourceLoc{line = 1, col = 17}})
      , (TReturn, Span{start = SourceLoc{line = 1, col = 18}, end = SourceLoc{line = 1, col = 24}})
      , (TLit 0, Span{start = SourceLoc{line = 1, col = 25}, end = SourceLoc{line = 1, col = 26}})
      , (TSemicolon, Span{start = SourceLoc{line = 1, col = 26}, end = SourceLoc{line = 1, col = 27}})
      , (TRBrace, Span{start = SourceLoc{line = 1, col = 28}, end = SourceLoc{line = 1, col = 29}})
      ]

lexCase2 :: UnitTest [(Token, Span)]
lexCase2 =
  UnitTest "match multiline program with comments" program Lexer.lex result
 where
  program =
    intercalate
      "\n"
      [ "// this is a program"
      , "int main(void) {"
      , "  int a = 80 >> 2 | 1 ^ 5 & 7 << 1;"
      , "  return a;"
      , "}"
      , "// trailing comment"
      , "/* multiline"
      , " * trailing"
      , " * comment"
      , " */"
      ]
  result =
    pure
      [ (TSingleLineComment "// this is a program", Span{start = SourceLoc{line = 1, col = 1}, end = SourceLoc{line = 1, col = 21}})
      , (TInt, Span{start = SourceLoc{line = 2, col = 1}, end = SourceLoc{line = 2, col = 4}})
      , (TIdent "main", Span{start = SourceLoc{line = 2, col = 5}, end = SourceLoc{line = 2, col = 9}})
      , (TLParen, Span{start = SourceLoc{line = 2, col = 9}, end = SourceLoc{line = 2, col = 10}})
      , (TVoid, Span{start = SourceLoc{line = 2, col = 10}, end = SourceLoc{line = 2, col = 14}})
      , (TRParen, Span{start = SourceLoc{line = 2, col = 14}, end = SourceLoc{line = 2, col = 15}})
      , (TLBrace, Span{start = SourceLoc{line = 2, col = 16}, end = SourceLoc{line = 2, col = 17}})
      , (TInt, Span{start = SourceLoc{line = 3, col = 3}, end = SourceLoc{line = 3, col = 6}})
      , (TIdent "a", Span{start = SourceLoc{line = 3, col = 7}, end = SourceLoc{line = 3, col = 8}})
      , (TEq, Span{start = SourceLoc{line = 3, col = 9}, end = SourceLoc{line = 3, col = 10}})
      , (TLit 80, Span{start = SourceLoc{line = 3, col = 11}, end = SourceLoc{line = 3, col = 13}})
      , (TRShift, Span{start = SourceLoc{line = 3, col = 14}, end = SourceLoc{line = 3, col = 16}})
      , (TLit 2, Span{start = SourceLoc{line = 3, col = 17}, end = SourceLoc{line = 3, col = 18}})
      , (TOr, Span{start = SourceLoc{line = 3, col = 19}, end = SourceLoc{line = 3, col = 20}})
      , (TLit 1, Span{start = SourceLoc{line = 3, col = 21}, end = SourceLoc{line = 3, col = 22}})
      , (TXor, Span{start = SourceLoc{line = 3, col = 23}, end = SourceLoc{line = 3, col = 24}})
      , (TLit 5, Span{start = SourceLoc{line = 3, col = 25}, end = SourceLoc{line = 3, col = 26}})
      , (TAnd, Span{start = SourceLoc{line = 3, col = 27}, end = SourceLoc{line = 3, col = 28}})
      , (TLit 7, Span{start = SourceLoc{line = 3, col = 29}, end = SourceLoc{line = 3, col = 30}})
      , (TLShift, Span{start = SourceLoc{line = 3, col = 31}, end = SourceLoc{line = 3, col = 33}})
      , (TLit 1, Span{start = SourceLoc{line = 3, col = 34}, end = SourceLoc{line = 3, col = 35}})
      , (TSemicolon, Span{start = SourceLoc{line = 3, col = 35}, end = SourceLoc{line = 3, col = 36}})
      , (TReturn, Span{start = SourceLoc{line = 4, col = 3}, end = SourceLoc{line = 4, col = 9}})
      , (TIdent "a", Span{start = SourceLoc{line = 4, col = 10}, end = SourceLoc{line = 4, col = 11}})
      , (TSemicolon, Span{start = SourceLoc{line = 4, col = 11}, end = SourceLoc{line = 4, col = 12}})
      , (TRBrace, Span{start = SourceLoc{line = 5, col = 1}, end = SourceLoc{line = 5, col = 2}})
      , (TSingleLineComment "// trailing comment", Span{start = SourceLoc{line = 6, col = 1}, end = SourceLoc{line = 6, col = 20}})
      , (TMultiLineComment ["/* multiline", " * trailing", " * comment", " */"], Span{start = SourceLoc{line = 7, col = 1}, end = SourceLoc{line = 11, col = 4}})
      ]

spec :: Spec
spec = do
  describe "lexer" $ do
    let cases =
          [ lexCase1
          , lexCase2
          ]

    forM_ cases $ \(UnitTest caseName input run expectedResult) -> it ("case: " ++ caseName) $ do
      run input `shouldBe` expectedResult
