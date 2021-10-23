import Data.Maybe

import Control.Parse
import qualified Data.Table as T
import Util

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

main = do
  Options symbol path <- execParser parser
  files <- getFilesUnderFolderOrFile path
  symbs <- mapM symsFromFile files
  print symbs
  pure ()
  -- mod <- parseFromFile =<< getLine

  -- let syms = symsFromModule mod
  -- let fibLocs = syms T.!? "fib"
  -- putStrLn $ unlines $ map show fibLocs

  -- pure ()