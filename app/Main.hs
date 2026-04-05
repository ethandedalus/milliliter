module Main where

import Cli (Options (..), options)
import Codegen (lower)
import Codegen.Emit (emit)
import Control.Monad (unless)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriterT)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TL
import IR.Emit (lowerProgram)
import Lexer (runLexer)
import Lexer.Types (mkSourceLoc)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))
import Parser (runParser)

compile :: Options -> IO ()
compile (Options fileName stopAfterLex stopAfterParse stopAfterIR stopAfterCodegen out) = do
  program <- readFile fileName
  ts <- lex' program
  unless stopAfterLex $ do
    ast <- parse' ts

    unless stopAfterParse $ do
      irAST <- ir' ast

      unless stopAfterIR $ do
        asmAST <- codegen' irAST

        unless stopAfterCodegen $ do
          case runWriterT (emit asmAST) of
            Left err -> error (show err)
            Right (_, builder) -> unless stopAfterCodegen $ do
              TL.writeFile out $ toLazyText builder
  where
    lex' p = either (error . show) pure $ evalStateT runLexer (p, mkSourceLoc 1 1)
    parse' ts = either (error . show) pure $ evalStateT runParser ts
    ir' p = either (error . show) pure $ evalStateT (lowerProgram p) 0
    codegen' ast = either (error . show) pure $ lower ast

main :: IO ()
main = execParser opts >>= compile
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> header "milliliter - a minimal C compiler"
            <> progDesc "Milliliter is a tiny C Compiler"
        )
