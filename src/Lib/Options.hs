module Lib.Options(
    Options(..)
  , parseOpts
) where

import Data.Maybe (fromMaybe)

import Options.Applicative

data Options = Options
 { symbol :: String
 , path   :: FilePath
 } deriving (Show, Eq)

opts :: Parser Options
opts = Options
  <$> argument str (metavar "SYMBOL" <> help "symbol to search for")
  <*> (fromMaybe "." <$> optional (argument str (metavar "PATH" <> help "directory/file to search in")))

parser = info (opts <**> helper) (fullDesc <> progDesc "search for a symbol")

parseOpts :: IO Options
parseOpts = execParser parser
