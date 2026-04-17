{-# LANGUAGE DataKinds #-}

module Compiler (runCompiler) where

import Compiler.Cli (Options (Options), options)
import Compiler.Codegen (codegen)
import Compiler.Codegen.Emit (emit, program)
import qualified Compiler.IR as IR (transform)
import qualified Compiler.IR.Transform as IR (transformProgram)
import qualified Compiler.Lexer as Lexer (lex)
import qualified Compiler.Parser as P (parse)
import qualified Compiler.Parser.Combinators as P
import qualified Compiler.SemanticAnalysis as S (analyze, analyzeProgram)
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
      analyzed <- analyze' ast

      unless stopAfterSemanticAnalysis $ do
        irAST <- ir' analyzed
        unless stopAfterIR $ do
          asmAST <- codegen' irAST

          unless stopAfterCodegen $ do
            case out of
              Nothing -> error ""
              (Just out') -> do
                asm <- emit' asmAST
                TL.writeFile out' $ toLazyText asm
 where
  lift' = either (error . show)

  lex' p = lift' pure (Lexer.lex p)
  parse' ts = lift' pure $ P.parse P.parseProgram ts
  analyze' ast = lift' pure $ S.analyze S.analyzeProgram ast
  ir' p = lift' pure $ IR.transform IR.transformProgram p
  codegen' ast = lift' pure $ codegen ast
  emit' lowered = lift' pure $ emit program lowered

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
