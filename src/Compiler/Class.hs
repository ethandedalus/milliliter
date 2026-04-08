{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Class (From (..)) where

class From a b where
  from :: a -> b
