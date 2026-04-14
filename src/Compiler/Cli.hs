module Compiler.Cli (Options (..), options) where

import Options.Applicative (Parser, help, long, optional, short, strArgument, strOption, switch)

-- | Options can be used to configure the compilation process
data Options = Options
  { file :: String
  , lex :: Bool
  , parse :: Bool
  , validate :: Bool
  , ir :: Bool
  , codegen :: Bool
  , outFile :: Maybe String
  }

options :: Parser Options
options =
  Options
    <$> strArgument (help "the file to compile")
    <*> switch (long "lex" <> short 'l' <> help "stop after lexing")
    <*> switch (long "parse" <> short 'p' <> help "stop after parsing")
    <*> switch (long "validate" <> short 'v' <> help "stop after validating")
    <*> switch (long "tacky" <> short 't' <> help "stop after IR generation")
    <*> switch (long "codegen" <> short 'c' <> help "stop after codegen")
    <*> optional (strOption (long "out" <> short 'o' <> help "output file"))
