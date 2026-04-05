module Parser.Combinators where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, put)
import Lexer.Types (Token (..))
import Parser.Errors (unexpectedEOF, unexpectedToken)
import Parser.Types (Expr (..), Func (..), Parser, Program (..), Stmt (..), UnaryOperator (..))

consumeComments :: Parser ()
consumeComments = do
  stream <- get
  case stream of
    ((TSingleLineComment _, _) : ts) -> put ts >> consumeComments
    ((TMultiLineComment _, _) : ts) -> put ts >> consumeComments
    _ -> pure ()

consumeToken :: Token -> Parser ()
consumeToken expected =
  consumeComments >> do
    stream <- get
    case stream of
      ((t, _) : ts) | t == expected -> void (put ts)
      ((t, _) : _) -> throwError (unexpectedToken t (show expected))
      [] -> throwError (unexpectedEOF (show expected))

consumeTokens :: [Token] -> Parser ()
consumeTokens = mapM_ consumeToken

consumeIdent :: Parser String
consumeIdent =
  consumeComments >> do
    stream <- get
    case stream of
      ((TIdent ident, _) : ts) -> put ts >> return ident
      ((t, _) : _) -> throwError (unexpectedToken t "IDENT")
      [] -> throwError (unexpectedEOF "IDENT")

parseExpr :: Parser Expr
parseExpr =
  consumeComments >> do
    stream <- get
    case stream of
      ((TLiteral lit, _) : ts) -> put ts >> return (Lit lit)
      ((TNot, _) : ts) -> put ts >> Unary Complement <$> parseExpr
      ((TMinus, _) : ts) -> put ts >> Unary Negate <$> parseExpr
      ((TLParen, _) : ts) ->
        put ts >> do
          expr <- parseExpr
          consumeToken TRParen
          return expr
      [] -> throwError (unexpectedEOF "LITERAL")
      ((x, _) : _) -> throwError (unexpectedToken x "LITERAL")

parseStmt :: Parser Stmt
parseStmt =
  consumeComments >> do
    stream <- get
    case stream of
      ((TReturn, _) : ts) -> do
        put ts
        expr <- parseExpr
        consumeToken TSemicolon
        return (Return expr)
      [] -> throwError (unexpectedEOF "STMT")
      ((x, _) : _) -> throwError (unexpectedToken x "STMT")

parseFunc :: Parser Func
parseFunc =
  consumeComments >> do
    consumeToken TInt
    ident <- consumeIdent
    consumeTokens [TLParen, TVoid, TRParen, TLBrace]
    stmt <- parseStmt
    consumeToken TRBrace
    return $ Func ident stmt

parseProgram :: Parser Program
parseProgram =
  consumeComments >> do
    func <- parseFunc
    stream <- get
    case stream of
      [] -> return $ Program func
      ((x, _) : _) -> throwError (unexpectedToken x "PROGRAM")
