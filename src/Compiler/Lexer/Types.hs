{-# LANGUAGE LambdaCase #-}

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
  tokenName,
)
where

import Control.Monad.State (StateT)

data Token
  = TLit Int
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
  | TAndAnd
  | TOrOr
  | TGTE
  | TLTE
  | TGT
  | TLT
  | TComplement
  deriving (Show, Eq)

tokenName :: Token -> String
tokenName = \case
  TLit _ -> "INTEGER_LITERAL"
  TIdent _ -> "IDENT"
  TInt -> "INT"
  TVoid -> "VOID"
  TFloat -> "FLOAT"
  TReturn -> "RETURN"
  TLParen -> "OPEN_PAREN"
  TRParen -> "CLOSE_PAREN"
  TLBrace -> "OPEN_BRACE"
  TRBrace -> "CLOSE_BRACE"
  TLBrack -> "OPEN_BRACKET"
  TRBrack -> "CLOSE_BRACKET"
  TSemicolon -> "SEMICOLON"
  TComma -> "COMMA"
  TSingleLineComment _ -> "LINE_COMMENT"
  TMultiLineComment _ -> "BLOCK_COMMENT"
  TPlus -> "PLUS"
  TMinus -> "MINUS"
  TPlusPlus -> "PLUS_PLUS"
  TMinusMinus -> "MINUS_MINUS"
  TStar -> "STAR"
  TDiv -> "SLASH"
  TMod -> "PERCENT"
  TAnd -> "AMP"
  TOr -> "PIPE"
  TXor -> "CARET"
  TLShift -> "SHL"
  TRShift -> "SHR"
  TNot -> "BANG"
  TEq -> "EQ"
  TPlusEq -> "PLUS_EQ"
  TMinusEq -> "MINUS_EQ"
  TStarEq -> "STAR_EQ"
  TDivEq -> "SLASH_EQ"
  TEqEq -> "EQEQ"
  TNotEq -> "NEQ"
  TAndAnd -> "AND"
  TOrOr -> "OR"
  TGTE -> "GTE"
  TLTE -> "LTE"
  TGT -> "GT"
  TLT -> "LT"
  TComplement -> "TILDE"

data RuleMatch = RuleMatch {matchToken :: Token, matchRest :: String, matchLen :: Int} deriving (Show, Eq)

type Rule = String -> Maybe RuleMatch

data SourceLoc = SourceLoc {line :: Int, col :: Int} deriving (Show, Eq)

data Span = Span {start :: SourceLoc, end :: SourceLoc} deriving (Show, Eq)

mkSpan :: (Int, Int) -> (Int, Int) -> Span
mkSpan (s1, s2) (e1, e2) = Span (mkSourceLoc s1 s2) (mkSourceLoc e1 e2)

mkSourceLoc :: Int -> Int -> SourceLoc
mkSourceLoc = SourceLoc

mkNilSpan :: Span
mkNilSpan = mkSpan (0, 0) (0, 0)

data LexError = SyntaxError String SourceLoc | UnexpectedEOF String deriving (Show, Eq)

data LexerState = LexerState {_program :: String, _location :: SourceLoc} deriving (Eq, Show)

type Lexer a = StateT (String, SourceLoc) (Either LexError) a
