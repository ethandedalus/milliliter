module Shared.UnitTest where

import CompileError (CompileError)

data UnitTest a = UnitTest
  { name :: String
  , program :: String
  , expected :: Either CompileError a
  }
