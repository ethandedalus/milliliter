{-# LANGUAGE PatternSynonyms #-}

module Compiler.Parser.Combinators (
  parseFactor,
  parseExpr,
  parseProgram,
  parseStmt,
  parseFunc,
  parseDecl,
  parseBlockItem,
) where

import Compiler.Lexer.Types (Span, Token (..), tokenName)
import Compiler.Parser.Types (
  BinaryOperator (..),
  BlockItem (..),
  Decl (..),
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

rewritePostfix :: Factor -> Parser Factor
rewritePostfix factor = do
  next <- peek
  case next of
    (Just TPlusPlus) -> do
      discardToken
      return $ Unary PostfixIncrement factor
    (Just TMinusMinus) -> do
      discardToken
      return $ Unary PostfixDecrement factor
    _ -> return factor

parseFactor :: Parser Factor
parseFactor = do
  consumeComments
  stream <- get
  case stream of
    ((TLit lit, _) : ts) -> put ts >> return (Lit lit)
    ((TComplement, _) : ts) -> put ts >> Unary Complement <$> parseFactor
    ((TNot, _) : ts) -> put ts >> Unary Not <$> parseFactor
    ((TMinus, _) : ts) -> put ts >> Unary Negate <$> parseFactor
    ((TPlusPlus, _) : ts) -> put ts >> Unary PrefixIncrement <$> parseFactor
    ((TMinusMinus, _) : ts) -> put ts >> Unary PrefixDecrement <$> parseFactor
    ((TIdent ident, _) : ts) -> put ts >> rewritePostfix (Ident ident)
    ((TLParen, _) : ts) -> do
      put ts
      expr <- parseExpr 0
      consumeToken TRParen
      rewritePostfix (Expr expr)
    [] -> throwError (UnexpectedEOF "expected one of LITERAL, TILDE, BANG, MINUS, OPEN_PAREN")
    ((t, _) : _) -> throwError (UnexpectedToken t "expected one of LITERAL, TILDE, BANG, MINUS, OPEN_PAREN")

peekBinaryOperatorOrAssignment :: Parser (Maybe (BinaryOperator, Int))
peekBinaryOperatorOrAssignment = do
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
    (Just TEq) -> return . return $ (Assignment, 1)
    (Just TPlusEq) -> return . return $ (AddAssign, 1)
    (Just TMinusEq) -> return . return $ (SubAssign, 1)
    (Just TStarEq) -> return . return $ (MulAssign, 1)
    (Just TDivEq) -> return . return $ (DivAssign, 1)
    (Just TModEq) -> return . return $ (ModAssign, 1)
    (Just TAndEq) -> return . return $ (AndAssign, 1)
    (Just TOrEq) -> return . return $ (OrAssign, 1)
    (Just TXorEq) -> return . return $ (XorAssign, 1)
    (Just TLShiftEq) -> return . return $ (ShlAssign, 1)
    (Just TRShiftEq) -> return . return $ (ShrAssign, 1)
    _ -> return Nothing

parseExpr :: Int -> Parser Expr
parseExpr minPrecedence = do
  consumeComments
  left <- parseFactor
  go (Factor left)
 where
  go :: Expr -> Parser Expr
  go left = do
    next <- peekBinaryOperatorOrAssignment
    case next of
      (Just (Assignment, precedence)) | precedence >= minPrecedence -> assign precedence left
      (Just (op, precedence)) | precedence >= minPrecedence, isCompoundAssign op -> compoundAssign op precedence left
      (Just (op, precedence)) | precedence >= minPrecedence -> do
        discardToken
        right <- parseExpr (precedence + 1)
        go (Binary op left right)
      _ -> return left

  isCompoundAssign op =
    op
      `elem` [ AddAssign
             , SubAssign
             , MulAssign
             , DivAssign
             , ModAssign
             , AndAssign
             , OrAssign
             , XorAssign
             , ShlAssign
             , ShrAssign
             ]

  assign precedence' left = do
    discardToken
    right <- parseExpr precedence'
    go (Assign left right)

  compoundAssign op precedence' left = do
    discardToken
    right <- parseExpr precedence'
    go (CompoundAssign op left right)

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
    ((TSemicolon, _) : ts) -> put ts >> return Null
    _ -> do
      expr <- parseExpr 0
      consumeToken TSemicolon
      return $ ExprS expr

parseDecl :: Parser Decl
parseDecl = do
  consumeComments
  consumeToken TInt
  ident <- consumeIdent
  stream <- get

  case stream of
    ((TEq, _) : ts) -> do
      put ts
      expr <- parseExpr 0
      consumeToken TSemicolon
      return $ Decl ident (pure expr)
    ((TSemicolon, _) : ts) -> put ts >> return (Decl ident Nothing)
    ((t, _) : _) -> throwError (UnexpectedToken t "expected EQUAL or SEMICOLON")
    _ -> throwError (UnexpectedEOF "expected EQUAL or SEMICOLON")

parseBlockItem :: Parser BlockItem
parseBlockItem = do
  consumeComments
  tok <- peek >>= \t -> maybe (throwError $ UnexpectedEOF "expected block item") return t
  case tok of
    TInt -> D <$> parseDecl
    _ -> S <$> parseStmt

parseFunc :: Parser Func
parseFunc = do
  consumeComments
  consumeToken TInt
  ident <- consumeIdent
  consumeTokens [TLParen, TVoid, TRParen, TLBrace]
  Func ident <$> parseBody []
 where
  parseBody :: [BlockItem] -> Parser [BlockItem]
  parseBody items = do
    consumeComments
    stream <- get
    case stream of
      ((TRBrace, _) : ts) -> put ts >> return items
      _ -> do
        item <- parseBlockItem
        parseBody (items ++ [item])

parseProgram :: Parser Program
parseProgram = do
  consumeComments
  func <- parseFunc
  stream <- get
  case stream of
    [] -> return $ Program func
    ((t, _) : _) -> throwError (UnexpectedToken t "expected EOF")
