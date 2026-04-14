{-# LANGUAGE LambdaCase #-}
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

data UnaryOperator
  = Complement
  | Negate
  | Not
  | PrefixIncrement
  | PostfixIncrement
  | PrefixDecrement
  | PostfixDecrement
  deriving (Eq, Show)

instance From PT.UnaryOperator UnaryOperator where
  from = \case
    PT.Complement -> Complement
    PT.Negate -> Negate
    PT.Not -> Not
    PT.PrefixIncrement -> PrefixIncrement
    PT.PostfixIncrement -> PostfixIncrement
    PT.PrefixDecrement -> PrefixIncrement
    PT.PostfixDecrement -> PostfixDecrement

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
  | Assignment
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | ShlAssign
  | ShrAssign
  deriving (Eq, Show)

instance From PT.BinaryOperator BinaryOperator where
  from = \case
    PT.Add -> Add
    PT.Sub -> Sub
    PT.Mul -> Mul
    PT.Div -> Div
    PT.Mod -> Mod
    PT.LeftShift -> LeftShift
    PT.RightShift -> RightShift
    PT.BitAnd -> BitAnd
    PT.Xor -> Xor
    PT.BitOr -> BitOr
    PT.Equal -> Equal
    PT.NotEqual -> NotEqual
    PT.GreaterThan -> GreaterThan
    PT.GreaterOrEqual -> GreaterOrEqual
    PT.LessThan -> LessThan
    PT.LessOrEqual -> LessOrEqual
    PT.And -> And
    PT.Or -> Or
    PT.Assignment -> Assignment
    PT.AddAssign -> AddAssign
    PT.SubAssign -> SubAssign
    PT.MulAssign -> MulAssign
    PT.DivAssign -> DivAssign
    PT.ModAssign -> ModAssign
    PT.AndAssign -> AndAssign
    PT.OrAssign -> OrAssign
    PT.XorAssign -> XorAssign
    PT.ShlAssign -> ShlAssign
    PT.ShrAssign -> ShrAssign

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
