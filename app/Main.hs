module Main where

import Data.Bfk.Executer
import Data.Bfk.Parser
import Debug.Trace
import Options.Applicative
import System.IO

data SourceOption = File String | Inline String deriving (Show)

parseOption :: Parser SourceOption
parseOption = parseOptionFile <|> parseOptionInline
  where
    parseOptionFile :: Parser SourceOption
    parseOptionFile = File <$> strOption (long "file" <> short 'f' <> metavar "FILEPATH" <> help "Brainf**k source file path")

    parseOptionInline :: Parser SourceOption
    parseOptionInline = Inline <$> strOption (long "inline" <> short 'i' <> metavar "PROGRAM" <> help "(inline) Brainf**k program")

main :: IO ()
main = do
  opts <- execParser optWithDesc
  executeSource opts
  where
    optWithDesc = info (parseOption <**> helper) (fullDesc <> progDesc "interpret Brainf**k source code file or inline source code" <> header "hbfk: Brainf**k interpreter written by Haskell")
    executeSource (File filename) = do
      program <- readFile filename
      parseAndExecuteInstructions program
    executeSource (Inline program) = parseAndExecuteInstructions program
    parseAndExecuteInstructions str = do
      case parseInstructions str of
        Right insts -> executeInstructions insts
        Left err -> hPrint stderr err
