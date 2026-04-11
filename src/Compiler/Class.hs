{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Class (From (..), ConversionError (..)) where

newtype ConversionError = ConversionError String deriving (Eq, Show)

class From a b where
  from :: a -> b
