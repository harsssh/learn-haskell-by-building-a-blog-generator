module Main where

import Data.Maybe (fromMaybe)
import HsBlog qualified
import Options.Applicative

main :: IO ()
main = HsBlog.main

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving (Show)

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        (long "input" <> short 'i' <> metavar "FILE" <> help "Input file")

pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
  where
    parser =
      strOption
        (long "output" <> short 'o' <> metavar "FILE" <> help "Output file")

pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

pInputDir :: Parser FilePath
pInputDir =
  strOption
    (long "input" <> short 'i' <> metavar "DIRECTORY" <> help "Input directory")

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    (long "output" <> short 'o' <> metavar "DIRECTORY" <> help "Output directory")

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir
