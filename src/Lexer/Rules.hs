{-# LANGUAGE FlexibleContexts #-}

module Lexer.Rules
  ( lexLiteral,
    lexKeywordOrIdent,
    lexAnd,
    lexComma,
    lexSingleLineComment,
    lexMultiLineComment,
    lexPlus,
    lexPlusPlus,
    lexMinus,
    lexMinusMinus,
    lexEq,
    lexNotEq,
    lexEqEq,
    lexDivEq,
    lexStar,
    lexStarEq,
    lexLShift,
    lexRShift,
    lexNot,
    lexXor,
    lexLParen,
    lexRParen,
    lexLBrack,
    lexRBrack,
    lexLBrace,
    lexRBrace,
    lexMod,
    lexSemicolon,
    allRules,
  )
where

import qualified Compiler.Types as CT (Literal (..))
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Lexer.Types (Rule, RuleMatch (..), Token (..))
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt, makeRegexOpts, matchM, multiline, (=~))

lexLiteral :: Rule
lexLiteral stream =
  lexFloat stream
    <|> lexInt stream
    <|> lexBool stream
    <|> lexString stream
    <|> lexChar stream
  where
    lexInt s = case s =~ "^[0-9]+" :: (String, String, String) of
      ("", m, rest)
        | null rest || not (isAlphaNum (head rest) || head rest == '_') ->
            Just $ RuleMatch (TLiteral (CT.LiteralInt (read m))) rest (length m)
      _ -> Nothing

    lexFloat :: Rule
    lexFloat s = case s =~ "^[0-9]+\\.[0-9]+" :: (String, String, String) of
      ("", m, rest)
        | null rest || not (isAlphaNum (head rest) || head rest == '_') ->
            Just $ RuleMatch (TLiteral (CT.LiteralFloat (read m))) rest (length m)
      _ -> Nothing

    lexString :: Rule
    lexString s = case s =~ "^\"[^\"]*\"" :: (String, String, String) of
      ("", m, rest) -> Just $ RuleMatch (TLiteral (CT.LiteralString (init (tail m)))) rest (length m)
      _ -> Nothing

    lexChar :: Rule
    lexChar s = case s =~ "^'[^']'" :: (String, String, String) of
      ("", m, rest) -> Just $ RuleMatch (TLiteral (CT.LiteralChar (m !! 1))) rest (length m)
      _ -> Nothing

    lexBool :: Rule
    lexBool s = case s =~ "^(true|false)" :: (String, String, String) of
      ("", m, rest) -> Just $ RuleMatch (TLiteral (CT.LiteralBool (m == "true"))) rest (length m)
      _ -> Nothing

lexKeywordOrIdent :: Rule
lexKeywordOrIdent stream = case stream =~ "^[a-zA-Z_]([a-zA-Z0-9_])*([:space:])*" :: (String, String, String) of
  ("", match, rest) -> case match of
    "int" -> Just (RuleMatch TInt rest 3)
    "void" -> Just (RuleMatch TVoid rest 4)
    "float" -> Just (RuleMatch TFloat rest 5)
    "return" -> Just (RuleMatch TReturn rest 6)
    _ -> Just (RuleMatch (TIdent match) rest (length match))
  _ -> Nothing

lexMinus :: Rule
lexMinus stream = case stream =~ "^-" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TMinus rest 1
  _ -> Nothing

lexPlus :: Rule
lexPlus stream = case stream =~ "^\\+" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlus rest 1
  _ -> Nothing

lexMinusMinus :: Rule
lexMinusMinus stream = case stream =~ "^--" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TMinusMinus rest 2
  _ -> Nothing

lexPlusPlus :: Rule
lexPlusPlus stream = case stream =~ "^\\+\\+" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlusPlus rest 2
  _ -> Nothing

lexStar :: Rule
lexStar stream = case stream =~ "^\\*" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlus rest 1
  _ -> Nothing

lexDiv :: Rule
lexDiv stream = case stream =~ "^/" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlus rest 1
  _ -> Nothing

lexMod :: Rule
lexMod stream = case stream =~ "^%" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlus rest 1
  _ -> Nothing

lexLParen :: Rule
lexLParen stream = case stream =~ "^\\(" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLParen rest 1
  _ -> Nothing

lexRParen :: Rule
lexRParen stream = case stream =~ "^\\)" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TRParen rest 1
  _ -> Nothing

lexLBrace :: Rule
lexLBrace stream = case stream =~ "^{" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLBrace rest 1
  _ -> Nothing

lexRBrace :: Rule
lexRBrace stream = case stream =~ "^}" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TRBrace rest 1
  _ -> Nothing

lexLBrack :: Rule
lexLBrack stream = case stream =~ "^\\[" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLBrack rest 1
  _ -> Nothing

lexRBrack :: Rule
lexRBrack stream = case stream =~ "^\\]" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TRBrack rest 1
  _ -> Nothing

lexSemicolon :: Rule
lexSemicolon stream = case stream =~ "^;" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TSemicolon rest 1
  _ -> Nothing

lexComma :: Rule
lexComma stream = case stream =~ "^," :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TComma rest 1
  _ -> Nothing

lexSingleLineComment :: Rule
lexSingleLineComment stream = case stream =~ "^//.*" :: (String, String, String) of
  ("", match, rest) -> Just $ RuleMatch (TSingleLineComment match) rest (length match)
  _ -> Nothing

lexMultiLineComment :: Rule
lexMultiLineComment stream = case makeRegexOpts defaultCompOpt {multiline = False} defaultExecOpt "/\\*.*\\*/" `matchM` stream :: Maybe (String, String, String) of
  Just ("", match, rest) -> Just $ RuleMatch (TMultiLineComment match) rest (length match)
  _ -> Nothing

lexAnd :: Rule
lexAnd stream = case stream =~ "^&" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TAnd rest 1
  _ -> Nothing

lexOr :: Rule
lexOr stream = case stream =~ "^\\|" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TOr rest 1
  _ -> Nothing

lexXor :: Rule
lexXor stream = case stream =~ "^\\^" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TXor rest 1
  _ -> Nothing

lexLShift :: Rule
lexLShift stream = case stream =~ "^<<" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLShift rest 2
  _ -> Nothing

lexRShift :: Rule
lexRShift stream = case stream =~ "^>>" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TRShift rest 2
  _ -> Nothing

lexNot :: Rule
lexNot stream = case stream =~ "^~" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TNot rest 1
  _ -> Nothing

lexEq :: Rule
lexEq stream = case stream =~ "^=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TEq rest 1
  _ -> Nothing

lexPlusEq :: Rule
lexPlusEq stream = case stream =~ "^\\+=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlusEq rest 2
  _ -> Nothing

lexMinusEq :: Rule
lexMinusEq stream = case stream =~ "^-=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TMinusEq rest 2
  _ -> Nothing

lexStarEq :: Rule
lexStarEq stream = case stream =~ "^\\*=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TStarEq rest 2
  _ -> Nothing

lexDivEq :: Rule
lexDivEq stream = case stream =~ "^/=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TDivEq rest 2
  _ -> Nothing

lexEqEq :: Rule
lexEqEq stream = case stream =~ "^==" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TEqEq rest 2
  _ -> Nothing

lexNotEq :: Rule
lexNotEq stream = case stream =~ "^!=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TNotEq rest 2
  _ -> Nothing

allRules :: [Rule]
allRules =
  [ lexSingleLineComment,
    lexMultiLineComment,
    lexLiteral,
    lexKeywordOrIdent,
    lexMinus,
    lexPlus,
    lexStar,
    lexDiv,
    lexMod,
    lexLBrace,
    lexRBrace,
    lexLParen,
    lexRParen,
    lexLBrack,
    lexRBrack,
    lexAnd,
    lexOr,
    lexXor,
    lexLShift,
    lexRShift,
    lexNot,
    lexPlusEq,
    lexMinusEq,
    lexStarEq,
    lexDivEq,
    lexEq,
    lexEqEq,
    lexNotEq,
    lexSemicolon,
    lexComma
  ]
