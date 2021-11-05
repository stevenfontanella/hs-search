import Control.Parse
import qualified Data.Table as T
import Util
import Lib.Format
import Lib.Options

import Data.Either (partitionEithers)
import System.FilePath
import System.IO

import Data.SymTable

import Debug.Trace


findOne :: String -> FilePath -> Mod -> SymTable
findOne toFind path = foldMapMod $ \info name ->
  if name == toFind
    then singleton path info
    else mempty

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
  Options symbol path <- parseOpts
  files <- filter ((== ".hs") . takeExtension) <$> getFilesUnderFolderOrFile path

  r <- results symbol files
  formatLn r
