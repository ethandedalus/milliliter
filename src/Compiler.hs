module Compiler (runCompiler) where

import Compiler.Cli (Options (Options), options)
import Compiler.Codegen (codegen)
import Compiler.Codegen.Emit (emit, program)
import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformProgram)
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as Parser (parse)
import Compiler.Parser.Combinators (parseProgram)
import qualified Compiler.SemanticAnalysis as S (analyze)
import qualified Compiler.SemanticAnalysis.VariableResolution as S (resolveProgram)
import Control.Monad (unless)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TL
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))

compile :: Options -> IO ()
compile (Options fileName stopAfterLex stopAfterParse stopAfterSemanticAnalysis stopAfterIR stopAfterCodegen out) = do
  program' <- readFile fileName
  ts <- lex' program'
  unless stopAfterLex $ do
    ast <- parse' ts

    unless stopAfterParse $ do
      analyzedAST <- analyze' ast

      unless stopAfterSemanticAnalysis $ do
        irAST <- ir' analyzedAST

        unless stopAfterIR $ do
          asmAST <- codegen' irAST

          unless stopAfterCodegen $ do
            case out of
              Nothing -> error ""
              (Just out') -> do
                asm <- emit' asmAST
                TL.writeFile out' $ toLazyText asm
 where
  lex' p = either (error . show) pure $ Lexer.lex p
  parse' ts = either (error . show) pure $ Parser.parse parseProgram ts
  analyze' ast = either (error . show) pure $ S.analyze S.resolveProgram ast
  ir' p = either (error . show) pure $ IR.transform IR.transformProgram p
  codegen' ast = either (error . show) pure $ codegen ast
  emit' lowered = either (error . show) pure $ emit program lowered

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
