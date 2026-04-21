module Compiler.Test.Shared.UnitTest (UnitTest (..), Binding (..), prepareEnv) where

import Compiler.AST
import Compiler.Error (CompileError (..))
import qualified Compiler.SemanticAnalysis.Resolve as S
import qualified Compiler.SemanticAnalysis.Types as S
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execStateT)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Compiler.Stage (Stage)

-- UnitTest identifies a test and encodes the compilation pipeline to run and well as the expected result
data UnitTest a = UnitTest
  { name :: String
  , program :: String
  , pipeline :: Stage String a
  , expected :: Either CompileError a
  }

bindTemp ::
  Int ->
  String ->
  b ->
  S.Transform b
bindTemp sid varName expr = do
  tmp <- S.makeTemporary varName
  S.varMap . at sid . non Map.empty . at varName .= Just tmp
  return expr

data Binding = Binding ScopeID String

prepareEnv :: [Binding] -> (S.ScopeContext, S.SemanticAnalysisState, Map.Map String String)
prepareEnv bindings = either (error . show) id $ do
  s <- execStateT (runReaderT bind ctx0) sas0
  let names =
        Map.fromList
          [ ( name'
            , fromMaybe (error $ "missing: " ++ name') $
                s ^. S.varMap . at sid . non Map.empty . at name'
            )
          | Binding sid name' <- bindings
          ]
  return (ctx, s, names)
 where
  ctx0 = S.ScopeContext{S._scopeStack = [], S._func = Nothing, S._controlStack = []}
  sas0 =
    S.SemanticAnalysisState
      { S._varSeq = 0
      , S._varMap = mempty
      , S._scopeSeq = 0
      , S._funcLabels = mempty
      , S._controlSeq = 0
      , S._switchCases = mempty
      , S._switchDefault = mempty
      }
  scopes = nub [sid | Binding sid _ <- bindings] -- dedup scopes
  ctx = ctx0 & S.scopeStack .~ scopes -- set up scopes
  bind = forM_ bindings $ \(Binding sid name') -> bindTemp sid name' ()
