{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Compiler.Parser.Types where

import qualified Compiler.Lexer.Types as LT (Span, Token)
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState, StateT (StateT), runStateT)

data ParseError
  = UnexpectedToken LT.Token String
  | UnexpectedEOF String
  | IllegalDeclAfterCase String
  | ParseError String
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: StateT [(LT.Token, LT.Span)] (Either ParseError) a}
  deriving (Functor, Applicative, Monad, MonadState [(LT.Token, LT.Span)], MonadError ParseError)

instance Alternative Parser where
  empty = Parser $ StateT $ \_ -> Left (ParseError "empty")
  p <|> q = Parser $ StateT $ \s ->
    case runStateT (runParser p) s of
      Left _ -> runStateT (runParser q) s
      Right x -> Right x

instance MonadPlus Parser
