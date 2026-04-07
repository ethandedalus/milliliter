module Parser.ParseFactorSpec where

import Control.Monad (forM_)
import Control.Monad.State (evalStateT)
import Parser.Combinators (parseFactor)
import Parser.ParseFactorTestCases (UnitTest (UnitTest), unaryFactorTestCases)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "unary factor expressions" $ do
    forM_ unaryFactorTestCases $ \(UnitTest caseName input expected) ->
      it ("case: " ++ caseName) $ evalStateT parseFactor input `shouldBe` expected
