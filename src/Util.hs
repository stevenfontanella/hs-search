module Util where

import Control.Monad
import System.Directory
import System.FilePath

import Debug.Trace

concatMapM :: (Traversable t, Applicative f) => (a1 -> f [a2]) -> t a1 -> f [a2]
concatMapM f = fmap concat . traverse f

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb then_ else_ = do
  b <- mb
  if b then then_ else else_

-- falses, trues
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM select = foldM f ([], [])
  where
    f (falses, trues) x =
      ifM (select x)
        (pure (falses, x:trues))
        (pure (x:falses, trues))

getFilesRec :: FilePath -> IO [FilePath]
getFilesRec path = do
    children <- map (path </>) <$> listDirectory path
    (dirs, files) <- partitionM doesFileExist children
    rec <- concatMapM getFilesRec dirs
    pure $ files ++ rec
    