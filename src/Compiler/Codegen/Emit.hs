module Compiler.Codegen.Emit where

import Compiler.Codegen.Fmt (
  fdecl,
  fstart,
  indented,
  instruction,
  instructions,
  prologue,
 )
import Compiler.Codegen.Types (Emitter, Func (..), Instruction (..), Program (..))
import qualified Compiler.Error as CE (CompileError (CodegenError))
import Compiler.Stage (Stage)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (execWriterT)
import Data.Bifunctor (first)
import Data.Text.Lazy.Builder (Builder)

func :: Func -> Emitter ()
func (Func name instructions' stackAlloc) = do
  fdecl name
  fstart name
  indented $ do
    prologue
    instruction $ StackAlloc stackAlloc
    instructions instructions'

program :: Program -> Emitter ()
program (Program f) = func f

emit :: (i -> Emitter a) -> Stage i Builder
emit f x = first CE.CodegenError q
 where
  q = execWriterT (runReaderT (f x) 0)
