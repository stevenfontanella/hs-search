import Debug.Trace

import Control.Parse
import qualified Data.Table as T

main = do
  mod <- parseFromFile =<< getLine

  let syms = symsFromModule mod
  let fibLocs = syms T.!? "fib"
  putStrLn $ unlines $ map show fibLocs

  pure ()