{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Types where

import Compiler.Class (From (..))
import Compiler.Types (Literal)
import Control.Monad.State (StateT)
import IR.Errors (IRError)
import qualified Parser.Types as PT (UnaryOperator (..))

data Val = Lit Literal | Var String deriving (Show, Eq)

data UnaryOperator = Complement | Negate deriving (Eq, Show)

instance From PT.UnaryOperator UnaryOperator where
  from PT.Complement = Complement
  from PT.Negate = Negate

data Instruction = Return Val | Unary UnaryOperator Val Val deriving (Show, Eq)

data Func = Func String [Instruction] deriving (Show, Eq)

newtype Program = Program Func deriving (Show, Eq)

data AST = ProgramNode Program | FuncNode Func | InstructionNode Instruction | ValNode Val deriving (Show, Eq)

type Emitter a = StateT Int (Either IRError) a
