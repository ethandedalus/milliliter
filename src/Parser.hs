module Parser (runParser) where

import Parser.Combinators (parseProgram)
import Parser.Types (Parser, Program)

runParser :: Parser Program
runParser = parseProgram
