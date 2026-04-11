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
  | TMultiLineComment [String]
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

mkSpan :: (Int, Int) -> (Int, Int) -> Span
mkSpan (s1, s2) (e1, e2) = Span (mkSourceLoc s1 s2) (mkSourceLoc e1 e2)

mkSourceLoc :: Int -> Int -> SourceLoc
mkSourceLoc = SourceLoc

mkNilSpan :: Span
mkNilSpan = mkSpan (0, 0) (0, 0)

type Lexer a = StateT (String, SourceLoc) (Either LexError) a
