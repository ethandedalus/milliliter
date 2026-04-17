module Compiler.Pass where

-- | Pass represents a compiler pass
data Pass
  = Parsed
  | Resolved
  | Typed
  deriving (Eq, Show)
