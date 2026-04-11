{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.IR.Types (
  Val (..),
  UnaryOperator (..),
  Instruction (..),
  Func (..),
  Program (..),
  Transform,
  BinaryOperator (..),
  IRState (..),
  varSeq,
  labelSeq,
  IRError (..),
) where

import Compiler.Class (From (..))
import qualified Compiler.Parser.Types as PT (BinaryOperator (..), UnaryOperator (..))
import Control.Lens
import Control.Monad.State (StateT)

data Val = Lit Int | Var String deriving (Show, Eq)

data UnaryOperator = Complement | Negate | Not deriving (Eq, Show)

instance From PT.UnaryOperator UnaryOperator where
  from PT.Complement = Complement
  from PT.Negate = Negate
  from PT.Not = Not

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | LeftShift
  | RightShift
  | BitAnd
  | Xor
  | BitOr
  | Equal
  | NotEqual
  | GreaterThan
  | GreaterOrEqual
  | LessThan
  | LessOrEqual
  | And
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
  from PT.BitAnd = BitAnd
  from PT.Xor = Xor
  from PT.BitOr = BitOr
  from PT.Equal = Equal
  from PT.NotEqual = NotEqual
  from PT.GreaterThan = GreaterThan
  from PT.GreaterOrEqual = GreaterOrEqual
  from PT.LessThan = LessThan
  from PT.LessOrEqual = LessOrEqual
  from PT.And = And
  from PT.Or = Or

data Instruction
  = Return Val
  | Unary UnaryOperator Val Val
  | Binary BinaryOperator Val Val Val
  | Copy Val Val
  | Jump String
  | JumpIfZero Val String
  | JumpIfNonZero Val String
  | Label String
  deriving (Show, Eq)

data Func = Func String [Instruction] deriving (Show, Eq)

newtype Program = Program Func deriving (Show, Eq)

newtype IRError = IRError String deriving (Show, Eq)

data IRState = IRState
  { _varSeq :: Int
  , _labelSeq :: Int
  }

makeLenses ''IRState

type Transform a = StateT IRState (Either IRError) a
