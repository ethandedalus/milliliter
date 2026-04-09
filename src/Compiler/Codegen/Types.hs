{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Codegen.Types where

import Compiler.Class (ConversionError (..), From (..), TryFrom (..))
import qualified Compiler.IR.Types as IR (BinaryOperator (..), UnaryOperator (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import qualified Data.Map as Map (Map)
import Data.Text.Lazy.Builder (Builder)

data UnaryOperator = Complement | Negate deriving (Eq, Show)

data BinaryOperator = Add | Sub | Mul deriving (Eq, Show)

instance TryFrom IR.BinaryOperator BinaryOperator where
  tryFrom IR.Add = pure Add
  tryFrom IR.Sub = pure Sub
  tryFrom IR.Mul = pure Mul
  tryFrom IR.Div = Left $ ConversionError "div cannot be lowered directly"
  tryFrom IR.Mod = Left $ ConversionError "mod cannot be lowered directly"

instance From IR.UnaryOperator UnaryOperator where
  from IR.Complement = Complement
  from IR.Negate = Negate

data Register = AX | DX | R10 | R11 deriving (Eq, Show)

data Operand = Imm Int | Register Register | Pseudo String | Stack Int deriving (Eq, Show)

data Instruction
  = Mov Operand Operand
  | Unary UnaryOperator Operand
  | Binary BinaryOperator Operand Operand
  | IDiv Operand
  | CDQ
  | StackAlloc Int
  | Ret
  deriving (Eq, Show)

data Func = Func String [Instruction] Int deriving (Eq, Show)

newtype Program = Program Func deriving (Eq, Show)

data CodegenError
  = IllegalOperand Operand String
  | IllegalInstruction Instruction String
  | ConvertBinaryOperator ConversionError
  | CodegenError String
  deriving (Show, Eq)

type Emitter a = ReaderT Int (WriterT Builder (Either CodegenError)) a

newtype PseudoRegistersPassError = PseudoRegistersPassError String deriving (Eq, Show)

data PseudoRegistersPassState = PseudoRegistersPassState
  { stackOffsets :: Map.Map String Int
  , currentOffset :: Int
  }
  deriving (Eq, Show)

type Transform s a = StateT s (Either CodegenError) a
