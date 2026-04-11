{-# LANGUAGE PatternSynonyms #-}

module Compiler.Parser.Combinators (parseFactor, parseExpr, parseProgram, parseStmt, parseFunc) where

import Compiler.Lexer.Types (Span, Token (..), tokenName)
import Compiler.Parser.Types (
  BinaryOperator (..),
  Expr (..),
  Factor (..),
  Func (..),
  ParseError (..),
  Parser,
  Program (..),
  Stmt (..),
  UnaryOperator (..),
 )
import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, put)

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
    ((t, _) : _) -> throwError (UnexpectedToken t $ "expected " ++ tokenName expected)
    [] -> throwError (UnexpectedEOF $ "expected " ++ tokenName expected)

discardToken :: Parser ()
discardToken = do
  consumeComments
  stream <- get
  case stream of
    ((_, _) : ts) -> put ts
    _ -> throwError (UnexpectedEOF "end of stream")

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
    ((t, _) : _) -> throwError (UnexpectedToken t $ "expected " ++ "IDENT")
    [] -> throwError (UnexpectedEOF "expected IDENT")

parseFactor :: Parser Factor
parseFactor = do
  consumeComments
  stream <- get
  case stream of
    ((TLit lit, _) : ts) -> put ts >> return (Lit lit)
    ((TComplement, _) : ts) -> put ts >> Unary Complement <$> parseFactor
    ((TNot, _) : ts) -> put ts >> Unary Not <$> parseFactor
    ((TMinus, _) : ts) -> put ts >> Unary Negate <$> parseFactor
    ((TLParen, _) : ts) ->
      put ts >> do
        expr <- parseExpr 0
        consumeToken TRParen
        return $ Expr expr
    [] -> throwError (UnexpectedEOF "expected one of LITERAL, TILDE, BANG, MINUS, OPEN_PAREN")
    ((t, _) : _) -> throwError (UnexpectedToken t "expected one of LITERAL, TILDE, BANG, MINUS, OPEN_PAREN")

peekBinaryOperator :: Parser (Maybe (BinaryOperator, Int))
peekBinaryOperator = do
  consumeComments
  tok <- peek
  case tok of
    (Just TStar) -> return . return $ (Mul, 130)
    (Just TDiv) -> return . return $ (Div, 130)
    (Just TMod) -> return . return $ (Mod, 130)
    (Just TPlus) -> return . return $ (Add, 120)
    (Just TMinus) -> return . return $ (Sub, 120)
    (Just TLShift) -> return . return $ (LeftShift, 110)
    (Just TRShift) -> return . return $ (RightShift, 110)
    (Just TGT) -> return . return $ (GreaterThan, 100)
    (Just TLT) -> return . return $ (LessThan, 100)
    (Just TGTE) -> return . return $ (GreaterOrEqual, 100)
    (Just TLTE) -> return . return $ (LessOrEqual, 100)
    (Just TEqEq) -> return . return $ (Equal, 90)
    (Just TNotEq) -> return . return $ (NotEqual, 90)
    (Just TAnd) -> return . return $ (BitAnd, 80)
    (Just TXor) -> return . return $ (Xor, 70)
    (Just TOr) -> return . return $ (BitOr, 60)
    (Just TAndAnd) -> return . return $ (And, 50)
    (Just TOrOr) -> return . return $ (Or, 40)
    _ -> return Nothing

parseExpr :: Int -> Parser Expr
parseExpr minPrecedence = do
  consumeComments
  left <- parseFactor
  go (Factor left)
 where
  binaryOperators =
    [ Mul
    , Div
    , Mod
    , Add
    , Sub
    , LeftShift
    , RightShift
    , GreaterThan
    , LessThan
    , GreaterOrEqual
    , LessOrEqual
    , Equal
    , NotEqual
    , BitAnd
    , Xor
    , BitOr
    , And
    , Or
    ]
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
    [] -> throwError (UnexpectedEOF $ "expected " ++ tokenName TReturn)
    ((t, _) : _) -> throwError (UnexpectedToken t $ "expected " ++ tokenName TReturn)

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
  consumeComments
  stream <- get
  case stream of
    [] -> return $ Program func
    ((t, _) : _) -> throwError (UnexpectedToken t "expected EOF")
