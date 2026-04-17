{-# LANGUAGE TemplateHaskell #-}

module Compiler.SemanticAnalysis.Types where

import Compiler.AST (ScopeID)
import Control.Lens
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import qualified Data.Map as Map
import Data.Set (Set)

data SemanticAnalysisState = SemanticAnalysisState
  { _varSeq :: Int
  , _varMap :: Map.Map ScopeID (Map.Map String String)
  , _scopeSeq :: Int
  , _funcLabels :: Map.Map String (Set String)
  }

makeLenses ''SemanticAnalysisState

data ScopeContext = ScopeContext
  { _scopeStack :: [ScopeID]
  , _func :: Maybe String
  }

makeLenses ''ScopeContext

data SemanticAnalysisError
  = InvalidLValue
  | UndefinedVariable String
  | Redefinition String
  | InvalidExpressionType String
  | SemanticAnalysisError String
  | IllegalLabel String String
  | DuplicateLabel String String
  deriving (Eq, Show)

type Transform a = ReaderT ScopeContext (StateT SemanticAnalysisState (Either SemanticAnalysisError)) a
