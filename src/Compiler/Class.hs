{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Class (From (..), into) where

class From a b where
  from :: a -> b

into :: (From a b) => a -> b
into = from
