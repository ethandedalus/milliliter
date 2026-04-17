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

import qualified Compiler.AST as AST
import Compiler.Class (From (..))
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

instance From AST.UnaryOperator UnaryOperator where
  from = \case
    AST.Complement -> Complement
    AST.Negate -> Negate
    AST.Not -> Not
    AST.PrefixIncrement -> PrefixIncrement
    AST.PostfixIncrement -> PostfixIncrement
    AST.PrefixDecrement -> PrefixIncrement
    AST.PostfixDecrement -> PostfixDecrement

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

instance From AST.BinaryOperator BinaryOperator where
  from = \case
    AST.Add -> Add
    AST.Sub -> Sub
    AST.Mul -> Mul
    AST.Div -> Div
    AST.Mod -> Mod
    AST.LeftShift -> LeftShift
    AST.RightShift -> RightShift
    AST.BitAnd -> BitAnd
    AST.Xor -> Xor
    AST.BitOr -> BitOr
    AST.Equal -> Equal
    AST.NotEqual -> NotEqual
    AST.GreaterThan -> GreaterThan
    AST.GreaterOrEqual -> GreaterOrEqual
    AST.LessThan -> LessThan
    AST.LessOrEqual -> LessOrEqual
    AST.And -> And
    AST.Or -> Or
    AST.Assignment -> Assignment
    AST.AddAssign -> AddAssign
    AST.SubAssign -> SubAssign
    AST.MulAssign -> MulAssign
    AST.DivAssign -> DivAssign
    AST.ModAssign -> ModAssign
    AST.AndAssign -> AndAssign
    AST.OrAssign -> OrAssign
    AST.XorAssign -> XorAssign
    AST.ShlAssign -> ShlAssign
    AST.ShrAssign -> ShrAssign
    binop -> error $ "ICE: operator " ++ show binop ++ " cannot be converted directly into IR"

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
