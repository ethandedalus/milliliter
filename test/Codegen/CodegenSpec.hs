module Codegen.CodegenSpec where

import Codegen (lower)
import Codegen.Cases (cases)
import Control.Monad (forM_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "lower" $ do
    forM_ cases $ \(name, input, expected) -> it ("case: " ++ name) $ lower input `shouldBe` expected
