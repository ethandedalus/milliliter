{-# LANGUAGE FlexibleContexts #-}

module Lexer (runLexer) where

import Control.Monad.Except (throwError)
import Control.Monad.State (get, put)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Lexer.Rules (allRules)
import Lexer.Types (LexError (..), Lexer, Rule, RuleMatch (..), SourceLoc (..), Span (..), Token (..), mkSourceLoc, mkSpan)

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
advance rules =
  advanceSourceLoc >> do
    (program, loc) <- get
    case mapMaybe ($ program) rules of
      [] -> throwError (UnexpectedEOF $ "could not match any token: " ++ program)
      matches -> do
        let (RuleMatch tok res len) = maximumBy (compare `on` matchLen) matches
        let tokenEndLoc = mkSourceLoc (line loc) (col loc + len)
        put (res, tokenEndLoc)
        return (tok, mkSpan loc tokenEndLoc)

runLexer :: Lexer [(Token, Span)]
runLexer = advanceSourceLoc >> get >>= \(program, _) -> if program == "" then return [] else advance allRules >>= \r -> (r :) <$> runLexer
