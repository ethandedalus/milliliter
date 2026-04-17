{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Compiler.AST where

import Compiler.Pass

data Type = IntT | UnsignedIntT deriving (Eq, Show)

data UnaryOperator
  = Complement
  | Negate
  | Not
  | PrefixIncrement
  | PostfixIncrement
  | PrefixDecrement
  | PostfixDecrement
  deriving (Eq, Show)

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | BitAnd
  | BitOr
  | Xor
  | LeftShift
  | RightShift
  | And
  | Or
  | Equal
  | NotEqual
  | GreaterThan
  | GreaterOrEqual
  | LessThan
  | LessOrEqual
  | Assignment
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | ShlAssign
  | ShrAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | Conditional
  deriving (Eq, Show)

type ScopeID = Int

data Symbol = Symbol {symName :: String, symScopeID :: ScopeID, symID :: String}

type family XLit (p :: Pass)
type family XVar (p :: Pass) where
  XVar Resolved = Int
  XVar Typed = Int
  XVar _ = ()

type family XAssign (p :: Pass)

type family XCompoundAssign (p :: Pass)

type family XConditionalE (p :: Pass)

type family XUnary (p :: Pass)

type family XBinary (p :: Pass)

type family XBlock (p :: Pass) where
  XBlock Resolved = ScopeID
  XBlock Typed = ScopeID
  XBlock _ = ()

type family XLabel (p :: Pass) where
  XLabel Resolved = String
  XLabel Typed = String
  XLabel _ = ()

type family XGoto (p :: Pass) where
  XGoto Resolved = String
  XGoto Typed = String
  XGoto _ = ()

-- TODO: Add source spans on data extensions for every AST node
data Expr (p :: Pass)
  = Lit Int
  | Var (XVar p) String
  | Binary BinaryOperator (Expr p) (Expr p)
  | Assign (Expr p) (Expr p)
  | CompoundAssign BinaryOperator (Expr p) (Expr p)
  | ConditionalE (Expr p) (Expr p) (Expr p)
  | Unary UnaryOperator (Expr p)

type ParsedExpr = Expr Parsed
type ResolvedExpr = Expr Resolved
type TypedExpr = Expr Typed

data Stmt (p :: Pass)
  = Return (Expr p)
  | ExprS (Expr p)
  | Null
  | If (Expr p) (Stmt p) (Maybe (Stmt p))
  | Compound (Block p)
  | Label (XLabel p) String (Stmt p)
  | Goto (XGoto p) String

type ParsedStmt = Stmt Parsed
type ResolvedStmt = Stmt Resolved
type TypedStmt = Stmt Typed

data Decl (p :: Pass)
  = Decl String (Maybe (Expr p))

type ParsedDecl = Decl Parsed
type ResolvedDecl = Decl Resolved
type TypedDecl = Decl Typed

data BlockItem (p :: Pass)
  = S (Stmt p)
  | D (Decl p)

type ParsedBlockItem = BlockItem Parsed
type ResolvedBlockItem = BlockItem Resolved
type TypedBlockItem = BlockItem Typed

data Block (p :: Pass) = Block (XBlock p) [BlockItem p]

type ParsedBlock = Block Parsed
type ResolvedBlock = Block Resolved
type TypedBlock = Block Typed

data Func (p :: Pass)
  = Func {name :: String, body :: Block p}

type ParsedFunc = Func Parsed
type ResolvedFunc = Func Resolved
type TypedFunc = Func Typed

newtype Program (p :: Pass) = Program (Func p)

type ShowPass p = (Show (XBlock p), Show (XVar p), Show (XLabel p), Show (XGoto p))
type EqPass p = (Eq (XBlock p), Eq (XVar p), Eq (XLabel p), Eq (XGoto p))

deriving instance (ShowPass p) => Show (Expr p)
deriving instance (ShowPass p) => Show (Block p)
deriving instance (ShowPass p) => Show (Stmt p)
deriving instance (ShowPass p) => Show (Decl p)
deriving instance (ShowPass p) => Show (BlockItem p)
deriving instance (ShowPass p) => Show (Func p)
deriving instance (ShowPass p) => Show (Program p)

deriving instance (EqPass p) => Eq (Expr p)
deriving instance (EqPass p) => Eq (Block p)
deriving instance (EqPass p) => Eq (Stmt p)
deriving instance (EqPass p) => Eq (Decl p)
deriving instance (EqPass p) => Eq (BlockItem p)
deriving instance (EqPass p) => Eq (Func p)
deriving instance (EqPass p) => Eq (Program p)

pattern VarParsed :: String -> Expr Parsed
pattern VarParsed x = Var () x

pattern VarResolved :: String -> Expr Resolved
pattern VarResolved x = Var 0 x

pattern BlockParsed :: [BlockItem Parsed] -> Block Parsed
pattern BlockParsed items = Block () items

pattern GotoParsed :: String -> Stmt Parsed
pattern GotoParsed ident = Goto () ident

pattern LabelParsed :: String -> Stmt Parsed -> Stmt Parsed
pattern LabelParsed ident stmt = Label () ident stmt
