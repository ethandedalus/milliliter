module Compiler.Parser.Errors (
  ParseError (..),
  unexpectedEOF,
  unexpectedToken,
)
where

import Compiler.Lexer.Types (Token (..))

data ParseError
  = UnexpectedToken Token String
  | UnexpectedEOF String
  | ParseError String
  deriving (Show, Eq)

unexpectedEOF :: String -> ParseError
unexpectedEOF ctx = UnexpectedEOF ("expected " ++ ctx)

unexpectedToken :: Token -> String -> ParseError
unexpectedToken tok ctx = UnexpectedToken tok (show tok ++ ", expected " ++ ctx)
