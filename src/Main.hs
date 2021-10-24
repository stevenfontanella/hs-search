import Control.Parse
import qualified Data.Table as T
import Util
import Lib.Format

import Data.Maybe
import System.FilePath

import Options.Applicative hiding (Mod)

import Data.SymTable

data Options = Options
 { symbol :: String
 , path   :: FilePath
 } deriving (Show, Eq)

opts :: Parser Options
opts = Options
  <$> argument str (metavar "SYMBOL" <> help "symbol to search for")
  <*> (fromMaybe "." <$> optional (argument str (metavar "PATH" <> help "directory/file to search in")))

parser = info (opts <**> helper) (fullDesc <> progDesc "search for a symbol")

findOne :: String -> FilePath -> Mod -> SymTable
findOne toFind path = foldMapMod 
  (\info name -> if name == toFind then singleton path info else mempty) 

results :: String -> [FilePath] -> IO SymTable
results toFind paths = do
  mods <- mapM parseFromFile paths
  pure $ mconcat $ zipWith (findOne toFind) paths mods

main = do
  Options symbol path <- execParser parser
  files <- filter ((== ".hs") . takeExtension)<$> getFilesUnderFolderOrFile path

  r <- results symbol files
  formatLn r
