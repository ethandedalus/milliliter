module Compiler.Test.Shared.UnitTest (UnitTest (..)) where

import Compiler.Error (CompileError (..))
import Compiler.Stage (Stage)

-- UnitTest identifies a test and encodes the compilation pipeline to run and well as the expected result
data UnitTest a = UnitTest
  { name :: String
  , program :: String
  , pipeline :: Stage String a
  , expected :: Either CompileError a
  }
