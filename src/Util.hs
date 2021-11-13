module Util where

import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath hiding ((<.>))

import Debug.Trace

(<.>) f = (fmap f .)

concatMapM :: (Traversable t, Applicative f) => (a -> f [b]) -> t a -> f [b]
concatMapM f = fmap concat . traverse f

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb then_ else_ = do
  b <- mb
  if b then then_ else else_

guardM :: (Monad m, Alternative m) => m Bool -> m ()
guardM mbool = guard =<< mbool

-- falses, trues
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM select = foldM f ([], [])
  where
    f (falses, trues) x =
      ifM (select x)
        (pure (falses, x:trues))
        (pure (x:falses, trues))

dfs :: (Monad m) => (a -> m [a]) -> (a -> m [b]) -> a -> m [b]
dfs neighbors yield curr = do
  succs <- neighbors curr
  liftA2 (++) (yield curr) $ concatMapM (dfs neighbors yield) succs

getFilesUnderFolderOrFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getFilesUnderFolderOrFile exclude = dfs succs yield
  where
    succs path = ifM (doesDirectoryExist path)
                     (filter (not . exclude) <$> (map (path </>) <$> listDirectory path))
                     (pure [])
    yield path = ifM (doesFileExist path)
                     (pure [path])
                     (pure [])

implies :: Bool -> Bool -> Bool
implies x y = y || not x
