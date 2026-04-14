{-# LANGUAGE TemplateHaskell #-}

module Compiler.SemanticAnalysis.Types where

import Control.Lens
import Control.Monad.State (StateT)
import qualified Data.Map as Map

data SemanticAnalysisState = SemanticAnalysisState {_varSeq :: Int, _varMap :: Map.Map String String}

makeLenses ''SemanticAnalysisState

data SemanticAnalysisError
  = InvalidLValue
  | UndefinedVariable String
  | Redefinition String
  | SemanticAnalysisError String
  deriving (Eq, Show)

type Transform a = StateT SemanticAnalysisState (Either SemanticAnalysisError) a
