{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
type LoopID = Int
type SwitchID = Int

data ControlID = LoopID LoopID | SwitchID SwitchID deriving (Eq, Show)

data Symbol = Symbol {symName :: String, symScopeID :: ScopeID, symID :: String}

data SwitchData = SwitchData {switchID :: SwitchID, caseValues :: [Int], hasDefault :: Bool} deriving (Eq, Show)

-- | XLit
type family XLit (p :: Pass)

type instance XLit _ = ()

-- | XVar
type family XVar (p :: Pass)

type instance XVar Parsed = ()
type instance XVar Resolved = ()
type instance XVar Typed = ()

-- | XAssign
type family XAssign (p :: Pass)

-- | XCompoundAssign
type family XCompoundAssign (p :: Pass)

-- | XConditionalE
type family XConditionalE (p :: Pass)

-- | XUnary
type family XUnary (p :: Pass)

-- | XBinary
type family XBinary (p :: Pass)

-- | XBlock
type family XBlock (p :: Pass)

type instance XBlock Parsed = ()
type instance XBlock Resolved = ScopeID
type instance XBlock Typed = ScopeID

-- | XLabel
type family XLabel (p :: Pass)

type instance XLabel Parsed = ()
type instance XLabel Resolved = String
type instance XLabel Typed = String

-- | XGoto
type family XGoto (p :: Pass)

type instance XGoto Parsed = ()
type instance XGoto Resolved = String
type instance XGoto Typed = String

-- | XWhile
type family XWhile (p :: Pass)

type instance XWhile Parsed = ()
type instance XWhile Resolved = LoopID
type instance XWhile Typed = LoopID

-- | XDoWhile
type family XDoWhile (p :: Pass)

type instance XDoWhile Parsed = ()
type instance XDoWhile Resolved = LoopID
type instance XDoWhile Typed = LoopID

-- | XFor
type family XFor (p :: Pass)

type instance XFor Parsed = ()
type instance XFor Resolved = LoopID
type instance XFor Typed = LoopID

-- | XBreak
type family XBreak (p :: Pass)

type instance XBreak Parsed = ()
type instance XBreak Resolved = ControlID
type instance XBreak Typed = ControlID

-- | XContinue
type family XContinue (p :: Pass)

type instance XContinue Parsed = ()
type instance XContinue Resolved = LoopID
type instance XContinue Typed = LoopID

-- | XSwitch
type family XSwitch (p :: Pass)

type instance XSwitch Parsed = ()
type instance XSwitch Resolved = SwitchData
type instance XSwitch Typed = SwitchData

type family XCase (p :: Pass)

type instance XCase Parsed = ()
type instance XCase Resolved = SwitchID
type instance XCase Typed = SwitchID

type family XDefault (p :: Pass)

type instance XDefault Parsed = ()
type instance XDefault Resolved = SwitchID
type instance XDefault Typed = SwitchID

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

data ForInit (p :: Pass) = InitDecl (Decl p) | InitExpr (Expr p) | Empty

data Stmt (p :: Pass)
  = Return (Expr p)
  | ExprS (Expr p)
  | Null
  | If (Expr p) (Stmt p) (Maybe (Stmt p))
  | Compound (Block p)
  | Label (XLabel p) String (Stmt p)
  | Goto (XGoto p) String
  | Break (XBreak p)
  | Continue (XContinue p)
  | While (XWhile p) (Expr p) (Stmt p)
  | DoWhile (XDoWhile p) (Stmt p) (Expr p)
  | For (XFor p) (ForInit p) (Maybe (Expr p)) (Maybe (Expr p)) (Stmt p)
  | Case (XCase p) (Expr p)
  | Switch (XSwitch p) (Expr p) (Stmt p)
  | Default (XDefault p)

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

-- Patterns

-- Var
pattern VarP :: String -> Expr Parsed
pattern VarP x = Var () x

pattern VarR :: String -> Expr Resolved
pattern VarR x = Var () x

pattern VarTy :: String -> Expr Typed
pattern VarTy x = Var () x

-- Label
pattern LabelP :: String -> Stmt Parsed -> Stmt Parsed
pattern LabelP ident stmt = Label () ident stmt

pattern LabelR :: String -> String -> Stmt Resolved -> Stmt Resolved
pattern LabelR x y stmt = Label x y stmt

pattern LabelTy :: String -> String -> Stmt Typed -> Stmt Typed
pattern LabelTy x y stmt = Label x y stmt

-- Goto

pattern GotoP :: String -> ParsedStmt
pattern GotoP ident = Goto () ident

pattern GotoR :: String -> String -> ResolvedStmt
pattern GotoR a b = Goto a b

pattern GotoTy :: String -> String -> Stmt Typed
pattern GotoTy a b = Goto a b

-- Block
pattern BlockP :: [BlockItem Parsed] -> Block Parsed
pattern BlockP items = Block () items

-- While
pattern WhileP :: Expr Parsed -> Stmt Parsed -> Stmt Parsed
pattern WhileP a b = While () a b

pattern WhileR :: LoopID -> Expr Resolved -> Stmt Resolved -> Stmt Resolved
pattern WhileR i a b = While i a b

-- Do While
pattern DoWhileP :: Stmt Parsed -> Expr Parsed -> Stmt Parsed
pattern DoWhileP a b = DoWhile () a b

pattern DoWhileR :: LoopID -> Stmt Resolved -> Expr Resolved -> Stmt Resolved
pattern DoWhileR i a b = DoWhile i a b

-- For
pattern ForP ::
  ForInit Parsed ->
  Maybe (Expr Parsed) ->
  Maybe (Expr Parsed) ->
  Stmt Parsed ->
  Stmt Parsed
pattern ForP a b c d = For () a b c d

pattern ForR ::
  LoopID ->
  ForInit Resolved ->
  Maybe (Expr Resolved) ->
  Maybe (Expr Resolved) ->
  Stmt Resolved ->
  Stmt Resolved
pattern ForR a b c d e = For a b c d e

-- Break
pattern BreakP :: Stmt Parsed
pattern BreakP = Break ()

pattern BreakR :: ControlID -> Stmt Resolved
pattern BreakR i = Break i

-- Continue
pattern ContinueP :: Stmt Parsed
pattern ContinueP = Continue ()

pattern ContinueR :: LoopID -> Stmt Resolved
pattern ContinueR i = Continue i

-- Case
pattern CaseP :: Expr Parsed -> Stmt Parsed
pattern CaseP e = Case () e

pattern CaseR :: SwitchID -> Expr Resolved -> Stmt Resolved
pattern CaseR i s = Case i s

pattern CaseTy :: SwitchID -> Expr Typed -> Stmt Typed
pattern CaseTy i s = Case i s

-- Switch
pattern SwitchP :: Expr Parsed -> Stmt Parsed -> Stmt Parsed
pattern SwitchP e s = Switch () e s

pattern SwitchR :: SwitchData -> Expr Resolved -> Stmt Resolved -> Stmt Resolved
pattern SwitchR i e s = Switch i e s

pattern SwitchTy :: SwitchData -> Expr Typed -> Stmt Typed -> Stmt Typed
pattern SwitchTy i e s = Switch i e s

-- Default
pattern DefaultP :: Stmt Parsed
pattern DefaultP = Default ()

pattern DefaultR :: SwitchID -> Stmt Resolved
pattern DefaultR i = Default i

pattern DefaultTy :: SwitchID -> Stmt Typed
pattern DefaultTy i = Default i

type ShowPass p =
  ( Show (XBlock p)
  , Show (XVar p)
  , Show (XLabel p)
  , Show (XGoto p)
  , Show (XWhile p)
  , Show (XDoWhile p)
  , Show (XFor p)
  , Show (XBreak p)
  , Show (XContinue p)
  , Show (XSwitch p)
  , Show (XCase p)
  , Show (XDefault p)
  )

type EqPass p =
  ( Eq (XBlock p)
  , Eq (XVar p)
  , Eq (XLabel p)
  , Eq (XGoto p)
  , Eq (XWhile p)
  , Eq (XDoWhile p)
  , Eq (XFor p)
  , Eq (XBreak p)
  , Eq (XContinue p)
  , Eq (XSwitch p)
  , Eq (XCase p)
  , Eq (XDefault p)
  )

deriving instance (ShowPass p) => Show (ForInit p)
deriving instance (ShowPass p) => Show (Expr p)
deriving instance (ShowPass p) => Show (Block p)
deriving instance (ShowPass p) => Show (Stmt p)
deriving instance (ShowPass p) => Show (Decl p)
deriving instance (ShowPass p) => Show (BlockItem p)
deriving instance (ShowPass p) => Show (Func p)
deriving instance (ShowPass p) => Show (Program p)

deriving instance (EqPass p) => Eq (ForInit p)
deriving instance (EqPass p) => Eq (Expr p)
deriving instance (EqPass p) => Eq (Block p)
deriving instance (EqPass p) => Eq (Stmt p)
deriving instance (EqPass p) => Eq (Decl p)
deriving instance (EqPass p) => Eq (BlockItem p)
deriving instance (EqPass p) => Eq (Func p)
deriving instance (EqPass p) => Eq (Program p)
