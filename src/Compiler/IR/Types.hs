{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.IR.Types (
  Val (..),
  UnaryOperator (..),
  Instruction (..),
  Func (..),
  Program (..),
  Emitter,
  BinaryOperator (..),
) where

import Compiler.Class (From (..))
import Compiler.IR.Errors (IRError)
import qualified Compiler.Parser.Types as PT (BinaryOperator (..), UnaryOperator (..))
import Compiler.Types (Literal)
import Control.Monad.State (StateT)

data Val = Lit Literal | Var String deriving (Show, Eq)

data UnaryOperator = Complement | Negate deriving (Eq, Show)

instance From PT.UnaryOperator UnaryOperator where
  from PT.Complement = Complement
  from PT.Negate = Negate

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | LeftShift
  | RightShift
  | And
  | Xor
  | Or
  deriving (Eq, Show)

instance From PT.BinaryOperator BinaryOperator where
  from PT.Add = Add
  from PT.Sub = Sub
  from PT.Mul = Mul
  from PT.Div = Div
  from PT.Mod = Mod
  from PT.LeftShift = LeftShift
  from PT.RightShift = RightShift
  from PT.And = And
  from PT.Xor = Xor
  from PT.Or = Or

data Instruction = Return Val | Unary UnaryOperator Val Val | Binary BinaryOperator Val Val Val deriving (Show, Eq)

data Func = Func String [Instruction] deriving (Show, Eq)

newtype Program = Program Func deriving (Show, Eq)

type Emitter a = StateT Int (Either IRError) a
