{-# LANGUAGE MultiParamTypeClasses #-}

module Codegen.Types where

import Compiler.Class (From, from)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import qualified Data.Map as Map (Map)
import Data.Text.Lazy.Builder (Builder)
import qualified IR.Types as IR (UnaryOperator (..))

data UnaryOperator = Complement | Negate deriving (Eq, Show)

instance From IR.UnaryOperator UnaryOperator where
  from IR.Complement = Complement
  from IR.Negate = Negate

data Register = AX | R10 deriving (Eq, Show)

data Operand = Imm Int | Register Register | Pseudo String | Stack Int deriving (Eq, Show)

data Instruction = Mov Operand Operand | Unary UnaryOperator Operand | StackAlloc Int | Ret deriving (Eq, Show)

data Func = Func String [Instruction] Int deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

data CodegenError = IllegalOperand Operand String | IllegalInstruction Instruction String | CodegenError String
  deriving (Show, Eq)

type Emitter a = WriterT Builder (Either CodegenError) a

newtype PseudoRegistersPassError = PseudoRegistersPassError String deriving (Eq, Show)

data PseudoRegistersPassState = PseudoRegistersPassState
  { stackOffsets :: Map.Map String Int,
    currentOffset :: Int
  }
  deriving (Eq, Show)

type Transform s a = StateT s (Either CodegenError) a
