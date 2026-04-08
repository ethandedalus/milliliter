{-# LANGUAGE PatternSynonyms #-}

module Parser.Combinators (parseFactor, parseExpr, parseProgram, parseStmt, parseFunc) where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, put)
import Lexer.Types (Span, Token (..))
import Parser.Errors (unexpectedEOF, unexpectedToken)
import Parser.Types (
  BinaryOperator (..),
  Expr (..),
  Factor (..),
  Func (..),
  Parser,
  Program (..),
  Stmt (..),
  UnaryOperator (..),
 )

pattern Tok :: Token -> [(Token, Span)] -> [(Token, Span)]
pattern Tok t ts <- ((t, _) : ts)

consumeComments :: Parser ()
consumeComments = do
  stream <- get
  case stream of
    ((TSingleLineComment _, _) : ts) -> put ts >> consumeComments
    ((TMultiLineComment _, _) : ts) -> put ts >> consumeComments
    _ -> pure ()

consumeToken :: Token -> Parser ()
consumeToken expected = do
  consumeComments
  stream <- get
  case stream of
    ((t, _) : ts) | t == expected -> void (put ts)
    ((t, _) : _) -> throwError (unexpectedToken t (show expected))
    [] -> throwError (unexpectedEOF (show expected))

discardToken :: Parser ()
discardToken = do
  consumeComments
  stream <- get
  case stream of
    ((_, _) : ts) -> put ts
    _ -> throwError (unexpectedEOF "eof")

consumeTokens :: [Token] -> Parser ()
consumeTokens = mapM_ consumeToken

peek :: Parser (Maybe Token)
peek = do
  consumeComments
  stream <- get
  case stream of
    Tok t _ -> (return . Just) t
    [] -> return Nothing
    _ -> return Nothing

consumeIdent :: Parser String
consumeIdent = do
  consumeComments
  stream <- get
  case stream of
    ((TIdent ident, _) : ts) -> put ts >> return ident
    ((t, _) : _) -> throwError (unexpectedToken t "IDENT")
    [] -> throwError (unexpectedEOF "IDENT")

parseFactor :: Parser Factor
parseFactor = do
  consumeComments
  stream <- get
  case stream of
    ((TLiteral lit, _) : ts) -> put ts >> return (Lit lit)
    ((TNot, _) : ts) -> put ts >> Unary Complement <$> parseFactor
    ((TMinus, _) : ts) -> put ts >> Unary Negate <$> parseFactor
    ((TLParen, _) : ts) ->
      put ts >> do
        expr <- parseExpr 0
        consumeToken TRParen
        return $ Expr expr
    [] -> throwError (unexpectedEOF "LITERAL")
    ((x, _) : _) -> throwError (unexpectedToken x "LITERAL")

peekBinaryOperator :: Parser (Maybe (BinaryOperator, Int))
peekBinaryOperator = do
  consumeComments
  tok <- peek
  case tok of
    (Just TPlus) -> return . return $ (Add, 45)
    (Just TMinus) -> return . return $ (Sub, 45)
    (Just TStar) -> return . return $ (Mul, 50)
    (Just TDiv) -> return . return $ (Div, 50)
    (Just TMod) -> return . return $ (Mod, 50)
    _ -> return Nothing

parseExpr :: Int -> Parser Expr
parseExpr minPrecedence = do
  consumeComments
  left <- parseFactor
  go (Factor left)
 where
  binaryOperators = [Add, Sub, Mul, Div, Mod]
  go left = do
    next <- peekBinaryOperator
    case next of
      (Just (op, precedence))
        | op `elem` binaryOperators
        , precedence >= minPrecedence -> do
            discardToken
            right <- parseExpr (precedence + 1)
            go (Binary op left right)
      _ -> return left

parseStmt :: Parser Stmt
parseStmt = do
  consumeComments
  stream <- get
  case stream of
    ((TReturn, _) : ts) -> do
      put ts
      expr <- parseExpr 0
      consumeToken TSemicolon
      return (Return expr)
    [] -> throwError (unexpectedEOF "STMT")
    ((x, _) : _) -> throwError (unexpectedToken x "STMT")

parseFunc :: Parser Func
parseFunc = do
  consumeComments
  consumeToken TInt
  ident <- consumeIdent
  consumeTokens [TLParen, TVoid, TRParen, TLBrace]
  stmt <- parseStmt
  consumeToken TRBrace
  return $ Func ident stmt

parseProgram :: Parser Program
parseProgram = do
  consumeComments
  func <- parseFunc
  stream <- get
  case stream of
    [] -> return $ Program func
    ((x, _) : _) -> throwError (unexpectedToken x "PROGRAM")
