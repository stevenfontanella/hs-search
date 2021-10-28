import Control.Parse
import qualified Data.Table as T
import Util
import Lib.Format

import Data.Maybe
import Data.Either (partitionEithers)
import System.FilePath
import System.IO

import System.Exit
import Control.Concurrent

import Options.Applicative hiding (Mod)

import Data.SymTable

import Debug.Trace

import System.Console.Isocline (putFmtLn)

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

putStderrLn :: String -> IO ()
putStderrLn = hPutStrLn stderr

results :: String -> [FilePath] -> IO SymTable
results toFind paths = do
  ~(errs, paths_mods) <- partitionEithers <$> mapM (\path -> fmap (path,) <$> parseFromFile path) paths
  mapM_ putStderrLn errs

  let (paths, mods) = unzip paths_mods
  -- pure $ mconcat $ withStrategy (parList rpar) $ zipWith (findOne toFind) paths mods
  pure $ mconcat $ zipWith (findOne toFind) paths mods

main = do
  -- installHandler keyboardSignal (Catch $ exitImmediately ExitSuccess) Nothing
  Options symbol path <- execParser parser
  files <- filter ((== ".hs") . takeExtension) <$> getFilesUnderFolderOrFile path

  r <- results symbol files
  formatLn r
