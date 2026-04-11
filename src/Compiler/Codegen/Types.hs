{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Codegen.Types where

import Compiler.Class (ConversionError (..), From (..))
import qualified Compiler.IR.Types as IR (Instruction, UnaryOperator (..))
import Control.Lens
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import qualified Data.Map as Map (Map)
import Data.Text.Lazy.Builder (Builder)

data UnaryOperator = Complement | Negate | Not deriving (Eq, Show)

data BinaryOperator = Add | Sub | Mul | LeftShift | RightShift | BitAnd | Xor | BitOr deriving (Eq, Show)

data CondCode = CondE | CondNE | CondG | CondGE | CondL | CondLE deriving (Eq, Show)

instance From IR.UnaryOperator UnaryOperator where
  from IR.Complement = Complement
  from IR.Negate = Negate
  from IR.Not = Not

data Register
  = AX
  | DX
  | R10
  | R11
  | R12
  | R13
  | R14
  | CX
  deriving (Eq, Show)

data Operand = Imm Int | Register Register | Pseudo String | Stack Int deriving (Eq, Show)

data Instruction
  = Mov Operand Operand
  | Unary UnaryOperator Operand
  | Binary BinaryOperator Operand Operand
  | IDiv Operand
  | CDQ
  | StackAlloc Int
  | Ret
  | Cmp Operand Operand
  | Jmp String
  | JmpCC CondCode String
  | SetCC CondCode Operand
  | Label String
  deriving (Eq, Show)

data Func = Func String [Instruction] Int deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

data CodegenError
  = IllegalOperand Operand String
  | UnlowerableInstruction IR.Instruction String
  | IllegalInstruction Instruction String
  | ConvertBinaryOperator ConversionError
  | CodegenError String
  deriving (Show, Eq)

type Emitter a = ReaderT Int (WriterT Builder (Either CodegenError)) a

newtype PseudoRegistersPassError = PseudoRegistersPassError String deriving (Eq, Show)

data PseudoRegistersPassState = PseudoRegistersPassState
  { _stackOffsets :: Map.Map String Int
  , _currentOffset :: Int
  }
  deriving (Eq, Show)

makeLenses ''PseudoRegistersPassState

type Transform s a = StateT s (Either CodegenError) a
