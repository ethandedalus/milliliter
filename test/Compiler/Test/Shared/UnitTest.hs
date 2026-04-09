{-# LANGUAGE DuplicateRecordFields #-}

module Compiler.Test.Shared.UnitTest (UnitTest (..)) where

import Compiler.Error (CompileError (..))

data UnitTest a = UnitTest
  { name :: String
  , program :: String
  , pipeline :: String -> Either CompileError a
  , expected :: Either CompileError a
  }
