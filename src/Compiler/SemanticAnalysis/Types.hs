{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.SemanticAnalysis.Types where

import Compiler.AST (ControlID, Expr, ScopeID, SwitchID)
import Compiler.Pass
import Control.Lens
import Control.Monad.Reader (ReaderT, local)
import Control.Monad.State (StateT)
import qualified Data.Map as Map
import Data.Set (Set)

data SemanticAnalysisState = SemanticAnalysisState
  { _varSeq :: Int
  , _varMap :: Map.Map ScopeID (Map.Map String String)
  , _scopeSeq :: Int
  , _funcLabels :: Map.Map String (Set String)
  , _switchCases :: Map.Map SwitchID (Set Int)
  , _switchDefault :: Map.Map SwitchID Bool
  , _controlSeq :: Int
  }

makeLenses ''SemanticAnalysisState

data ScopeContext = ScopeContext
  { _scopeStack :: [ScopeID]
  , _func :: Maybe String
  , _controlStack :: [ControlID]
  }

makeLenses ''ScopeContext

data SemanticAnalysisError
  = InvalidLValue (Expr Parsed)
  | UndefinedVariable String
  | VariableRedefinition String
  | InvalidExpressionType String
  | IllegalLabel String String
  | IllegalCaseExpression String
  | IllegalDefault String
  | DuplicateLabel String String
  | IllegalControlStatement String
  | SemanticAnalysisError String
  deriving (Eq, Show)

withContext :: ControlID -> Transform a -> Transform a
withContext cx = local (controlStack %~ (cx :))

withScope :: ScopeID -> Transform a -> Transform a
withScope sid = local (scopeStack %~ (sid :))

type Transform a = ReaderT ScopeContext (StateT SemanticAnalysisState (Either SemanticAnalysisError)) a
