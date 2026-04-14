{-# LANGUAGE FlexibleContexts #-}

module Compiler.Lexer.Rules where

import Compiler.Lexer.Types (Rule, RuleMatch (..), Token (..))
import Data.Char (isAlphaNum)
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt, makeRegexOpts, matchM, multiline, (=~))

lexLiteral :: Rule
lexLiteral = lexInt
 where
  lexInt s = case s =~ "^[0-9]+" :: (String, String, String) of
    ("", m, rest)
      | null rest || not (isAlphaNum (head rest) || head rest == '_') ->
          Just $ RuleMatch (TLit (read m)) rest (length m)
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

-- ARITHMETIC

lexPlus :: Rule
lexPlus stream = case stream =~ "^\\+" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlus rest 1
  _ -> Nothing

lexMinus :: Rule
lexMinus stream = case stream =~ "^-" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TMinus rest 1
  _ -> Nothing

lexStar :: Rule
lexStar stream = case stream =~ "^\\*" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TStar rest 1
  _ -> Nothing

lexDiv :: Rule
lexDiv stream = case stream =~ "^/" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TDiv rest 1
  _ -> Nothing

lexMod :: Rule
lexMod stream = case stream =~ "^%" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TMod rest 1
  _ -> Nothing

-- BITWISE

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

lexComplement :: Rule
lexComplement stream = case stream =~ "^~" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TComplement rest 1
  _ -> Nothing

lexLShift :: Rule
lexLShift stream = case stream =~ "^<<" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLShift rest 2
  _ -> Nothing

lexRShift :: Rule
lexRShift stream = case stream =~ "^>>" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TRShift rest 2
  _ -> Nothing

-- LOGICAL

lexAndAnd :: Rule
lexAndAnd stream = case stream =~ "^&&" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TAndAnd rest 2
  _ -> Nothing

lexOrOr :: Rule
lexOrOr stream = case stream =~ "^\\|\\|" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TOrOr rest 2
  _ -> Nothing

-- ASSIGN/COMPOUND ASSIGN

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

lexModEq :: Rule
lexModEq stream = case stream =~ "^%=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TModEq rest 2
  _ -> Nothing

lexAndEq :: Rule
lexAndEq stream = case stream =~ "^&=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TAndEq rest 2
  _ -> Nothing

lexOrEq :: Rule
lexOrEq stream = case stream =~ "^\\|=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TOrEq rest 2
  _ -> Nothing

lexXorEq :: Rule
lexXorEq stream = case stream =~ "^\\^=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TXorEq rest 2
  _ -> Nothing

lexLShiftEq :: Rule
lexLShiftEq stream = case stream =~ "^<<=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLShiftEq rest 3
  _ -> Nothing

lexRShiftEq :: Rule
lexRShiftEq stream = case stream =~ "^>>=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TRShiftEq rest 3
  _ -> Nothing

-- RELATIONAL

lexEqEq :: Rule
lexEqEq stream = case stream =~ "^==" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TEqEq rest 2
  _ -> Nothing

lexNotEq :: Rule
lexNotEq stream = case stream =~ "^!=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TNotEq rest 2
  _ -> Nothing

lexGT :: Rule
lexGT stream = case stream =~ "^>" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TGT rest 1
  _ -> Nothing

lexLT :: Rule
lexLT stream = case stream =~ "^<" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLT rest 1
  _ -> Nothing

lexGTE :: Rule
lexGTE stream = case stream =~ "^>=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TGTE rest 2
  _ -> Nothing

lexLTE :: Rule
lexLTE stream = case stream =~ "^<=" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TLTE rest 2
  _ -> Nothing

lexNot :: Rule
lexNot stream = case stream =~ "^!" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TNot rest 1
  _ -> Nothing

-- INCREMENT/DECREMENT

lexMinusMinus :: Rule
lexMinusMinus stream = case stream =~ "^--" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TMinusMinus rest 2
  _ -> Nothing

lexPlusPlus :: Rule
lexPlusPlus stream = case stream =~ "^\\+\\+" :: (String, String, String) of
  ("", _, rest) -> Just $ RuleMatch TPlusPlus rest 2
  _ -> Nothing

-- PUNCTUATION

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

-- COMMENTS

lexSingleLineComment :: Rule
lexSingleLineComment stream = case stream =~ "^//.*" :: (String, String, String) of
  ("", match, rest) -> Just $ RuleMatch (TSingleLineComment match) rest (length match)
  _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

lexMultiLineComment :: Rule
lexMultiLineComment stream = case makeRegexOpts defaultCompOpt{multiline = False} defaultExecOpt "/\\*.*\\*/" `matchM` stream :: Maybe (String, String, String) of
  Just ("", match, rest) -> Just $ RuleMatch (TMultiLineComment (splitOn '\n' match)) rest (length match)
  _ -> Nothing

allRules :: [Rule]
allRules =
  [ lexSingleLineComment
  , lexMultiLineComment
  , lexLiteral
  , lexKeywordOrIdent
  , lexMinus
  , lexPlus
  , lexStar
  , lexDiv
  , lexMod
  , lexLBrace
  , lexRBrace
  , lexLParen
  , lexRParen
  , lexLBrack
  , lexRBrack
  , lexAnd
  , lexOr
  , lexXor
  , lexLShift
  , lexRShift
  , lexComplement
  , lexAnd
  , lexNot
  , lexEq
  , lexSemicolon
  , lexComma
  , lexAndAnd
  , lexOrOr
  , lexGT
  , lexLT
  , lexGTE
  , lexLTE
  , lexEqEq
  , lexNotEq
  , lexPlusEq
  , lexMinusEq
  , lexStarEq
  , lexDivEq
  , lexModEq
  , lexAndEq
  , lexOrEq
  , lexXorEq
  , lexLShiftEq
  , lexRShiftEq
  , lexMinusMinus
  , lexPlusPlus
  ]
