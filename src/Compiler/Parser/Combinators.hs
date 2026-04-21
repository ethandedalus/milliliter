{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.Parser.Combinators where

import Compiler.AST
import qualified Compiler.Lexer.Types as LT
import Compiler.Parser.Types
import Compiler.Pass
import Control.Applicative (optional, (<|>))
import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (get, put)
import Data.Maybe (isJust)

consumeComments :: Parser ()
consumeComments = do
  stream <- get
  case stream of
    ((LT.TSingleLineComment _, _) : ts) -> put ts >> consumeComments
    ((LT.TMultiLineComment _, _) : ts) -> put ts >> consumeComments
    _ -> pure ()

consumeToken :: LT.Token -> Parser ()
consumeToken expected = do
  consumeComments
  stream <- get
  case stream of
    ((t, _) : ts) | t == expected -> void (put ts)
    ((t, _) : _) -> throwError (UnexpectedToken t $ "expected " ++ LT.tokenName expected)
    [] -> throwError (UnexpectedEOF $ "expected " ++ LT.tokenName expected)

discardToken :: Parser ()
discardToken = do
  consumeComments
  stream <- get
  case stream of
    ((_, _) : ts) -> put ts
    _ -> throwError (UnexpectedEOF "end of stream")

consumeTokens :: [LT.Token] -> Parser ()
consumeTokens = mapM_ consumeToken

peek :: Parser (Maybe LT.Token)
peek = do
  consumeComments
  stream <- get
  case stream of
    ((t, _) : _) -> (return . Just) t
    _ -> return Nothing

peek2 :: Parser (Maybe (LT.Token, LT.Token))
peek2 = do
  consumeComments
  stream <- get
  case stream of
    ((t1, _) : (t2, _) : _) -> (return . Just) (t1, t2)
    _ -> return Nothing

consumeIdent :: Parser String
consumeIdent = do
  consumeComments
  stream <- get
  case stream of
    ((LT.TIdent ident, _) : ts) -> put ts >> return ident
    ((t, _) : _) -> throwError (UnexpectedToken t $ "expected " ++ "IDENT")
    [] -> throwError (UnexpectedEOF "expected IDENT")

rewritePostfix :: Expr Parsed -> Parser (Expr Parsed)
rewritePostfix factor = do
  next <- peek
  case next of
    (Just LT.TPlusPlus) -> discardToken >> rewritePostfix (Unary PostfixIncrement factor)
    (Just LT.TMinusMinus) -> discardToken >> rewritePostfix (Unary PostfixDecrement factor)
    _ -> return factor

parseFactor :: Parser (Expr Parsed)
parseFactor = do
  consumeComments
  stream <- get
  case stream of
    ((LT.TLit lit, _) : ts) -> put ts >> return (Lit lit)
    ((LT.TComplement, _) : ts) -> put ts >> Unary Complement <$> parseFactor
    ((LT.TNot, _) : ts) -> put ts >> Unary Not <$> parseFactor
    ((LT.TMinus, _) : ts) -> put ts >> Unary Negate <$> parseFactor
    ((LT.TPlusPlus, _) : ts) -> put ts >> Unary PrefixIncrement <$> parseFactor
    ((LT.TMinusMinus, _) : ts) -> put ts >> Unary PrefixDecrement <$> parseFactor
    ((LT.TIdent ident, _) : ts) -> put ts >> rewritePostfix (VarP ident)
    ((LT.TLParen, _) : ts) -> do
      put ts
      expr <- parseExpr 0
      consumeToken LT.TRParen
      rewritePostfix expr
    [] -> throwError (UnexpectedEOF "expected one of LITERAL, TILDE, BANG, MINUS, OPEN_PAREN")
    ((t, _) : _) -> throwError (UnexpectedToken t "expected one of LITERAL, TILDE, BANG, MINUS, OPEN_PAREN")

peekBinaryOperatorOrAssignment :: Parser (Maybe (BinaryOperator, Int))
peekBinaryOperatorOrAssignment = do
  consumeComments
  tok <- peek
  case tok of
    Just LT.TStar -> return . return $ (Mul, 130)
    Just LT.TDiv -> return . return $ (Div, 130)
    Just LT.TMod -> return . return $ (Mod, 130)
    Just LT.TPlus -> return . return $ (Add, 120)
    Just LT.TMinus -> return . return $ (Sub, 120)
    Just LT.TLShift -> return . return $ (LeftShift, 110)
    Just LT.TRShift -> return . return $ (RightShift, 110)
    Just LT.TGT -> return . return $ (GreaterThan, 100)
    Just LT.TLT -> return . return $ (LessThan, 100)
    Just LT.TGTE -> return . return $ (GreaterOrEqual, 100)
    Just LT.TLTE -> return . return $ (LessOrEqual, 100)
    Just LT.TEqEq -> return . return $ (Equal, 90)
    Just LT.TNotEq -> return . return $ (NotEqual, 90)
    Just LT.TAnd -> return . return $ (BitAnd, 80)
    Just LT.TXor -> return . return $ (Xor, 70)
    Just LT.TOr -> return . return $ (BitOr, 60)
    Just LT.TAndAnd -> return . return $ (And, 50)
    Just LT.TOrOr -> return . return $ (Or, 40)
    Just LT.TQuestionMark -> return . return $ (Conditional, 3)
    Just LT.TEq -> return . return $ (Assignment, 1)
    Just LT.TPlusEq -> return . return $ (AddAssign, 1)
    Just LT.TMinusEq -> return . return $ (SubAssign, 1)
    Just LT.TStarEq -> return . return $ (MulAssign, 1)
    Just LT.TDivEq -> return . return $ (DivAssign, 1)
    Just LT.TModEq -> return . return $ (ModAssign, 1)
    Just LT.TAndEq -> return . return $ (AndAssign, 1)
    Just LT.TOrEq -> return . return $ (OrAssign, 1)
    Just LT.TXorEq -> return . return $ (XorAssign, 1)
    Just LT.TLShiftEq -> return . return $ (ShlAssign, 1)
    Just LT.TRShiftEq -> return . return $ (ShrAssign, 1)
    _ -> return Nothing

parseExpr :: Int -> Parser (Expr Parsed)
parseExpr minPrecedence = do
  consumeComments
  left <- parseFactor
  go left
 where
  go :: Expr Parsed -> Parser (Expr Parsed)
  go left = do
    next <- peekBinaryOperatorOrAssignment
    case next of
      (Just (Assignment, precedence)) | precedence >= minPrecedence -> assign precedence left
      (Just (op, precedence)) | precedence >= minPrecedence, isCompoundAssign op -> compoundAssign op precedence left
      Just (Conditional, precedence) | precedence >= minPrecedence -> do
        discardToken
        middle <- parseExpr 0
        consumeToken LT.TColon
        right <- parseExpr precedence
        go (ConditionalE left middle right)
      (Just (op, precedence)) | precedence >= minPrecedence -> do
        discardToken
        right <- parseExpr (precedence + 1)
        go (Binary op left right)
      _ -> return left

  isCompoundAssign = \case
    AddAssign -> True
    SubAssign -> True
    MulAssign -> True
    DivAssign -> True
    ModAssign -> True
    AndAssign -> True
    OrAssign -> True
    XorAssign -> True
    ShlAssign -> True
    ShrAssign -> True
    _ -> False

  assign precedence' left = do
    discardToken
    right <- parseExpr precedence'
    go (Assign left right)

  compoundAssign op precedence' left = do
    discardToken
    right <- parseExpr precedence'
    go (CompoundAssign op left right)

parseForInit :: Parser (ForInit Parsed)
parseForInit =
  InitExpr
    <$> (parseExpr 0 >>= \e -> consumeToken LT.TSemicolon >> return e)
    <|> InitDecl <$> parseDecl
    <|> (consumeToken LT.TSemicolon >> return Empty)

parseStmt :: Parser (Stmt Parsed)
parseStmt = do
  consumeComments
  stream <- get
  case stream of
    ((LT.TReturn, _) : ts) -> do
      put ts
      expr <- parseExpr 0
      consumeToken LT.TSemicolon
      return (Return expr)
    ((LT.TSemicolon, _) : ts) -> put ts >> return Null
    ((LT.TIf, _) : ts) -> do
      put ts
      consumeToken LT.TLParen
      cond <- parseExpr 0
      consumeToken LT.TRParen
      inner <- parseStmt
      next <- peek
      case next of
        Just LT.TElse -> discardToken >> If cond inner . Just <$> parseStmt
        _ -> return $ If cond inner Nothing
    ((LT.TLBrace, _) : _) -> Compound <$> parseBlock
    ((LT.TGoto, _) : ts) -> do
      put ts
      label <- consumeIdent
      consumeToken LT.TSemicolon
      return $ GotoP label
    ((LT.TIdent ident, _) : (LT.TColon, _) : ts) -> put ts >> LabelP ident <$> parseStmt
    ((LT.TBreak, _) : ts) -> put ts >> consumeToken LT.TSemicolon >> return BreakP
    ((LT.TContinue, _) : ts) -> put ts >> consumeToken LT.TSemicolon >> return ContinueP
    ((LT.TWhile, _) : ts) -> do
      put ts
      consumeToken LT.TLParen
      expr <- parseExpr 0
      consumeToken LT.TRParen
      WhileP expr <$> parseStmt
    ((LT.TDo, _) : ts) -> do
      put ts
      stmt <- parseStmt
      consumeTokens [LT.TWhile, LT.TLParen]
      expr <- parseExpr 0
      consumeTokens [LT.TRParen, LT.TSemicolon]
      return $ DoWhileP stmt expr
    ((LT.TFor, _) : ts) -> do
      put ts
      consumeToken LT.TLParen
      initClause <- parseForInit
      middleClause <- optional (parseExpr 0)
      consumeToken LT.TSemicolon
      lastClause <- optional (parseExpr 0)
      consumeToken LT.TRParen
      ForP initClause middleClause lastClause <$> parseStmt
    ((LT.TSwitch, _) : ts) -> do
      put ts
      consumeToken LT.TLParen
      expr <- parseExpr 0
      consumeToken LT.TRParen
      SwitchP expr <$> parseStmt
    ((LT.TCase, _) : ts) -> do
      put ts
      expr <- parseExpr 0
      consumeToken LT.TColon
      result <- (Just <$> parseDecl) `catchError` const (pure Nothing)
      when (isJust result) $ throwError $ IllegalDeclAfterCase "declarations cannot follow cases in C17"
      return $ CaseP expr
    ((LT.TDefault, _) : _) -> discardToken >> consumeToken LT.TColon >> return DefaultP
    _ -> do
      expr <- parseExpr 0
      consumeToken LT.TSemicolon
      return $ ExprS expr

parseDecl :: Parser (Decl Parsed)
parseDecl = do
  consumeComments
  consumeToken LT.TInt
  ident <- consumeIdent
  stream <- get

  case stream of
    ((LT.TEq, _) : ts) -> do
      put ts
      expr <- parseExpr 0
      consumeToken LT.TSemicolon
      return $ Decl ident (pure expr)
    ((LT.TSemicolon, _) : ts) -> put ts >> return (Decl ident Nothing)
    ((t, _) : _) -> throwError (UnexpectedToken t "expected EQUAL or SEMICOLON")
    _ -> throwError (UnexpectedEOF "expected EQUAL or SEMICOLON")

parseBlockItem :: Parser (BlockItem Parsed)
parseBlockItem = do
  consumeComments
  tok <- peek >>= \t -> maybe (throwError $ UnexpectedEOF "expected block item") return t
  case tok of
    LT.TInt -> D <$> parseDecl
    _ -> S <$> parseStmt

parseBlock :: Parser (Block Parsed)
parseBlock = do
  consumeComments
  consumeToken LT.TLBrace *> (BlockP <$> go []) <* consumeToken LT.TRBrace
 where
  go :: [BlockItem Parsed] -> Parser [BlockItem Parsed]
  go items = do
    consumeComments
    stream <- get
    case stream of
      ((LT.TRBrace, _) : _) -> return items
      _ -> parseBlockItem >>= \item -> go (items ++ [item])

parseFunc :: Parser (Func Parsed)
parseFunc = do
  consumeComments
  consumeToken LT.TInt
  ident <- consumeIdent
  consumeTokens [LT.TLParen, LT.TVoid, LT.TRParen]
  Func ident <$> parseBlock

parseProgram :: Parser (Program Parsed)
parseProgram = do
  consumeComments
  func <- parseFunc
  stream <- get
  case stream of
    [] -> consumeComments >> return (Program func)
    ((t, _) : _) -> throwError (UnexpectedToken t "expected EOF")
