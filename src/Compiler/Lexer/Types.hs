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
  | TLong Integer
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
  | TModEq
  | TEqEq
  | TNotEq
  | TAndAnd
  | TOrOr
  | TGTE
  | TLTE
  | TGT
  | TLT
  | TComplement
  | TAndEq
  | TOrEq
  | TLShiftEq
  | TRShiftEq
  | TXorEq
  | TColon
  | TIf
  | TElse
  | TQuestionMark
  | TGoto
  | TDo
  | TWhile
  | TFor
  | TBreak
  | TContinue
  | TSwitch
  | TCase
  | TDefault
  deriving (Show, Eq)

tokenName :: Token -> String
tokenName = \case
  TLit _ -> "INTEGER_LITERAL"
  TLong _ -> "LONG_LITERAL"
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
  TModEq -> "MOD_EQ"
  TAndEq -> "AND_EQ"
  TOrEq -> "OR_EQ"
  TLShiftEq -> "LEFT_SHIFT_EQ"
  TRShiftEq -> "RIGHT_SHIFT_EQ"
  TXorEq -> "XOR_EQ"
  TColon -> "COLON"
  TIf -> "IF"
  TElse -> "ELSE"
  TQuestionMark -> "QUESTION_MARK"
  TGoto -> "GOTO"
  TDo -> "DO"
  TWhile -> "WHILE"
  TFor -> "FOR"
  TBreak -> "BREAK"
  TContinue -> "CONTINUE"
  TSwitch -> "SWITCH"
  TCase -> "CASE"
  TDefault -> "DEFAULT"

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
