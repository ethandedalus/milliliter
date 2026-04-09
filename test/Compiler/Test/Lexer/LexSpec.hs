module Compiler.Test.Lexer.LexSpec where

import Compiler.Lexer.Rules (
  lexKeywordOrIdent,
  lexLBrace,
  lexLParen,
  lexLiteral,
  lexMinus,
  lexMinusMinus,
  lexMultiLineComment,
  lexPlus,
  lexPlusPlus,
  lexRBrace,
  lexRParen,
  lexSemicolon,
  lexSingleLineComment,
 )
import Compiler.Lexer.Types (RuleMatch (..), Token (..))
import qualified Compiler.Types as CT (Literal (..))
import Control.Monad (forM_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "lexKeywordOrIdent" $ do
    let cases =
          [ ("match keyword `int`", "int main", Just (RuleMatch TInt " main" 3))
          , ("match keyword `float`", "float x = 1.0;", Just (RuleMatch TFloat " x = 1.0;" 5))
          , ("match keyword `void`", "void func1(){}", Just (RuleMatch TVoid " func1(){}" 4))
          , ("match keyword `return`", "return 0;", Just (RuleMatch TReturn " 0;" 6))
          , ("match identifier `variableName`", "variableName = 42;", Just (RuleMatch (TIdent "variableName") " = 42;" 12))
          , ("match identifier `variable_name`", "variable_name = 42;", Just (RuleMatch (TIdent "variable_name") " = 42;" 13))
          , ("match identifier `variableName1`", "variableName1 = 42;", Just (RuleMatch (TIdent "variableName1") " = 42;" 13))
          , ("fail to match invalid identifier `1variable`", "1variable", Nothing)
          ]

    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexKeywordOrIdent input `shouldBe` expected

  describe "lexLiteral" $ do
    let cases =
          [ ("match literal int", "5;", Just (RuleMatch (TLiteral (CT.LiteralInt 5)) ";" 1))
          , ("match literal string", "\"hello, world\";", Just (RuleMatch (TLiteral (CT.LiteralString "hello, world")) ";" 14))
          , ("match literal float", "5.0;", Just (RuleMatch (TLiteral (CT.LiteralFloat 5.0)) ";" 3))
          , ("match literal bool", "true;", Just (RuleMatch (TLiteral (CT.LiteralBool True)) ";" 4))
          , ("fail to match [1]", "return 0;", Nothing)
          ]

    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexLiteral input `shouldBe` expected

  describe "lexLParen" $ do
    let cases =
          [ ("match left parenthesis [1]", "( void )", Just (RuleMatch TLParen " void )" 1))
          , ("match left parenthesis [2]", "(\nint arg1\n)", Just (RuleMatch TLParen "\nint arg1\n)" 1))
          , ("fail to match left brace [1]", "{ return 0; }", Nothing)
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexLParen input `shouldBe` expected

  describe "lexRParen" $ do
    let cases =
          [ ("match right parenthesis [1]", ") {", Just (RuleMatch TRParen " {" 1))
          , ("match right parenthesis [2]", ")\n{", Just (RuleMatch TRParen "\n{" 1))
          , ("fail to match identifier [1]", "abc)", Nothing)
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexRParen input `shouldBe` expected

  describe "lexLBrace" $ do
    let cases =
          [ ("match left brace [1]", "{return 0;}", Just (RuleMatch TLBrace "return 0;}" 1))
          , ("match left brace [2]", "{ return 0; }", Just (RuleMatch TLBrace " return 0; }" 1))
          , ("fail to match keyword `return`", "return 0;", Nothing)
          ]

    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexLBrace input `shouldBe` expected

  describe "lexRBrace" $ do
    let cases =
          [ ("match right brace [1]", "}", Just (RuleMatch TRBrace "" 1))
          , ("match right brace [2]", "}\n\n int main() {}", Just (RuleMatch TRBrace "\n\n int main() {}" 1))
          , ("fail to match keyword `int`", "int main() {}", Nothing)
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexRBrace input `shouldBe` expected

  describe "lexSemicolon" $ do
    let cases =
          [ ("match semicolon [1]", ";", Just (RuleMatch TSemicolon "" 1))
          , ("match semicolon [2]", ";\nreturn a;", Just (RuleMatch TSemicolon "\nreturn a;" 1))
          , ("fail to match keyword `return`", "return 0;", Nothing)
          ]

    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexSemicolon input `shouldBe` expected

  describe "lexSingleLineComment" $ do
    let cases =
          [ ("match single line comment [1]", "// @ingroup Physics", Just (RuleMatch (TSingleLineComment "// @ingroup Physics") "" 19))
          , ("match single line comment [2]", "//    comment\n//comment\n//comment", Just (RuleMatch (TSingleLineComment "//    comment") "\n//comment\n//comment" 13))
          , ("fail to match multiline comment", "/* some comment here */", Nothing)
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexSingleLineComment input `shouldBe` expected

  describe "lexMultiLineComment" $ do
    let cases =
          [ ("match multiline comment [1]", "/*multiline comment*///another comment", Just (RuleMatch (TMultiLineComment "/*multiline comment*/") "//another comment" 21))
          , ("match multiline comment [2]", "/**some  \ncomment  here  */", Just (RuleMatch (TMultiLineComment "/**some  \ncomment  here  */") "" 27))
          , ("fail to match unclosed multiline comment [1]", "/* *", Nothing)
          , ("fail to match unclosed multiline comment [2]", "/* /", Nothing)
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexMultiLineComment input `shouldBe` expected

  describe "lexPlus" $ do
    let cases =
          [ ("match plus [1]", "+ 1;", Just (RuleMatch TPlus " 1;" 1))
          , ("match plus [2]", "+10; return a;", Just (RuleMatch TPlus "10; return a;" 1))
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexPlus input `shouldBe` expected

  describe "lexPlusPlus" $ do
    let cases =
          [ ("match ++ [1]", "++1;", Just (RuleMatch TPlusPlus "1;" 2))
          , ("match ++ [2]", "++10; return a;", Just (RuleMatch TPlusPlus "10; return a;" 2))
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexPlusPlus input `shouldBe` expected

  describe "lexMinus" $ do
    let cases =
          [ ("match - [1]", "- 1;", Just (RuleMatch TMinus " 1;" 1))
          , ("match - [2]", "-10; return a;", Just (RuleMatch TMinus "10; return a;" 1))
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexMinus input `shouldBe` expected

  describe "lexMinusMinus" $ do
    let cases =
          [ ("match -- [1]", "--1;", Just (RuleMatch TMinusMinus "1;" 2))
          , ("match -- [2]", "--10; return a;", Just (RuleMatch TMinusMinus "10; return a;" 2))
          ]
    forM_ cases $ \(name, input, expected) ->
      it ("case: " ++ name) $ lexMinusMinus input `shouldBe` expected
