module Parser.ParseStmtSpec where

import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Parser.Combinators (parseStmt)
import Parser.ParseStmtTestCases (UnitTest (UnitTest), parseStmtTestCases)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parse literal expressions" $ do
    forM_ parseStmtTestCases $ \(UnitTest caseName input expected) ->
      it ("case: " ++ caseName) $ evalStateT parseStmt input `shouldBe` expected
