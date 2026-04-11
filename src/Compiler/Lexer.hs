{-# LANGUAGE FlexibleContexts #-}

module Compiler.Lexer (lex) where

import Prelude hiding (lex)

import qualified Compiler.Error as CE
import Compiler.Lexer.Rules (allRules)
import Compiler.Lexer.Types (
  LexError (..),
  Lexer,
  Rule,
  RuleMatch (..),
  SourceLoc (..),
  Span (..),
  Token (..),
  mkSourceLoc,
 )
import Compiler.Stage (Stage)
import Control.Monad.Except (throwError)
import Control.Monad.State (evalStateT, get, put)
import Data.Bifunctor (first)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)

advanceSourceLoc :: Lexer ()
advanceSourceLoc = do
  (program, loc) <- get
  case program of
    "" -> pure ()
    (x : xs) -> case x of
      ' ' -> put (xs, mkSourceLoc (line loc) (col loc + 1)) >> advanceSourceLoc
      '\t' -> put (xs, mkSourceLoc (line loc) (col loc + 1)) >> advanceSourceLoc
      '\n' -> put (xs, mkSourceLoc (line loc + 1) 1) >> advanceSourceLoc
      _ -> pure ()

advance :: [Rule] -> Lexer (Token, Span)
advance rules = do
  advanceSourceLoc
  (program, loc) <- get
  case mapMaybe ($ program) rules of
    [] -> throwError (SyntaxError ("could not match any token: " ++ take 20 program) loc)
    matches -> do
      let (RuleMatch tok res len) = maximumBy (compare `on` matchLen) matches
      let tokenEndLoc = case tok of
            (TMultiLineComment c) -> let final = tail c in mkSourceLoc (line loc + length c) (col loc + length final)
            _ -> mkSourceLoc (line loc) (col loc + len)
      put (res, tokenEndLoc)
      return (tok, Span loc tokenEndLoc)

runLexer :: Lexer [(Token, Span)]
runLexer = advanceSourceLoc >> get >>= \(program, _) -> if program == "" then return [] else advance allRules >>= \r -> (r :) <$> runLexer

lex :: Stage String [(Token, Span)]
lex program = first CE.LexError $ evalStateT runLexer (program, mkSourceLoc 1 1)
