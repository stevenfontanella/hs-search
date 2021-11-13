import Control.Parse
import qualified Data.Table as T
import Util
import Lib.Format
import Lib.Options
import Data.SymTable
import Data.Function

import Data.Char (toLower, isLower, isAlpha)
import Data.Either (partitionEithers)
import System.FilePath
import System.IO

import Debug.Trace

findOne :: (String -> Bool) -> FilePath -> Mod -> SymTable
findOne found path = foldMapMod $ \info name ->
  if found name
    then singleton path info
    else mempty

putStderrLn :: String -> IO ()
putStderrLn = hPutStrLn stderr

eqInsensitive :: String -> String -> Bool
eqInsensitive = (==) `on` map toLower

results :: (String -> Bool) -> [FilePath] -> IO SymTable
results found paths = do
  ~(errs, paths_mods) <- partitionEithers <$> mapM (\path -> fmap (path,) <$> parseFromFile path) paths
  mapM_ putStderrLn errs

  let (paths, mods) = unzip paths_mods
  -- pure $ mconcat $ withStrategy (parList rpar) $ zipWith (findOne toFind) paths mods
  pure $ mconcat $ zipWith (findOne found) paths mods

main = do
  -- installHandler keyboardSignal (Catch $ exitImmediately ExitSuccess) Nothing
  -- TODO: error if file/folder doesn't exists
  --       ignore .stack-work
  Options symbol path smartCase <- parseOpts
  files <- filter ((== ".hs") . takeExtension) <$> getFilesUnderFolderOrFile path
  let found target
        -- | True = True
        | all (\c -> isAlpha c `implies` isLower c) symbol
        , smartCase = target `eqInsensitive` symbol

        | otherwise = target == symbol

  r <- results found files
  formatLn r
