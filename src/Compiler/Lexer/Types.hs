module Compiler.Lexer.Types (
  Token (..),
  Rule,
  RuleMatch (..),
  SourceLoc (..),
  mkSpan,
  mkSourceLoc,
  Lexer,
  Span (..),
  LexError (..),
  mkNilSpan,
)
where

import qualified Compiler.Types as CT
import Control.Monad.State (StateT)

data Token
  = TLiteral CT.Literal
  | TIdent String
  | TInt
  | TVoid
  | TFloat
  | TReturn
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBrack
  | TRBrack
  | TSemicolon
  | TComma
  | TSingleLineComment String
  | TMultiLineComment String
  | TPlus
  | TMinus
  | TPlusPlus
  | TMinusMinus
  | TStar
  | TDiv
  | TMod
  | TAnd
  | TOr
  | TXor
  | TLShift
  | TRShift
  | TNot
  | TEq
  | TPlusEq
  | TMinusEq
  | TStarEq
  | TDivEq
  | TEqEq
  | TNotEq
  deriving (Show, Eq)

data RuleMatch = RuleMatch {matchToken :: Token, matchRest :: String, matchLen :: Int} deriving (Show, Eq)

type Rule = String -> Maybe RuleMatch

newtype LexError = UnexpectedEOF String deriving (Show, Eq)

data SourceLoc = SourceLoc {line :: Int, col :: Int} deriving (Show, Eq)

data Span = Span {start :: SourceLoc, end :: SourceLoc} deriving (Show, Eq)

mkSpan :: SourceLoc -> SourceLoc -> Span
mkSpan = Span

mkSourceLoc :: Int -> Int -> SourceLoc
mkSourceLoc = SourceLoc

mkNilSpan :: Span
mkNilSpan = mkSpan (mkSourceLoc 0 0) (mkSourceLoc 0 0)

type Lexer a = StateT (String, SourceLoc) (Either LexError) a
