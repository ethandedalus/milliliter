{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Class (From (..), TryFrom (..), ConversionError (..)) where

newtype ConversionError = ConversionError String deriving (Eq, Show)

class From a b where
  from :: a -> b

class TryFrom a b where
  tryFrom :: a -> Either ConversionError b
