module Compiler.Parser where

import qualified Compiler.Error as CE (CompileError (ParseError))
import Compiler.Lexer.Types (Span, Token)
import Compiler.Parser.Types (Parser, runParser)
import Compiler.Stage (Stage)
import Control.Monad.State (evalStateT)
import Data.Bifunctor (first)

parse :: Parser a -> Stage [(Token, Span)] a
parse parser tokens = first CE.ParseError $ evalStateT (runParser parser) tokens
