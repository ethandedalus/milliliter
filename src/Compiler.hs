module Compiler (runCompiler) where

import Compiler.Cli (Options (Options), options)
import Compiler.Codegen (codegen)
import Compiler.Codegen.Emit (emit, emitProgram)
import qualified Compiler.IR as IR (lower)
import qualified Compiler.IR.Lower as IR (lowerProgram)
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as Parser (parse)
import Compiler.Parser.Combinators (parseProgram)
import Control.Monad (unless)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TL
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))
import Prelude hiding (lex)

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
          asm <- emit' asmAST
          TL.writeFile out $ toLazyText asm
 where
  lex' p = either (error . show) pure $ Lexer.lex p
  parse' ts = either (error . show) pure $ Parser.parse parseProgram ts
  ir' p = either (error . show) pure $ IR.lower IR.lowerProgram p
  codegen' ast = either (error . show) pure $ codegen ast
  emit' lowered = either (error . show) pure $ emit emitProgram lowered

runCompiler :: IO ()
runCompiler = execParser opts >>= compile
 where
  opts =
    info
      (options <**> helper)
      ( fullDesc
          <> header "milliliter - a minimal C compiler"
          <> progDesc "Milliliter is a tiny C Compiler"
      )
