module Parser.ParseExprSpec where

import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Lexer.Types (Span (..), mkSourceLoc, mkSpan)
import Parser.Combinators (parseExpr)
import Parser.ParseExprTestCases (
  UnitTest (UnitTest),
  binaryExpressionsTestCases,
  literalExpressionsTestCases,
  unaryExpressionsTestCases,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

nilSpan :: Span
nilSpan = mkSpan (mkSourceLoc 0 0) (mkSourceLoc 0 0)

spec :: Spec
spec = do
  describe "parse literal expressions" $ do
    forM_ literalExpressionsTestCases $ \(UnitTest caseName input expected) ->
      it ("case: " ++ caseName) $ evalStateT (parseExpr 0) input `shouldBe` expected

  describe "parse unary expressions" $ do
    forM_ unaryExpressionsTestCases $ \(UnitTest caseName input expected) ->
      it ("case: " ++ caseName) $ evalStateT (parseExpr 0) input `shouldBe` expected

  describe "parse binary expressions" $ do
    forM_ binaryExpressionsTestCases $ \(UnitTest caseName input expected) ->
      it ("case: " ++ caseName) $ evalStateT (parseExpr 0) input `shouldBe` expected
