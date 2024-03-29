module Lib.Format (
    format
  , printFormatted
  , formatLn
) where

import Data.SymTable
import Util

import Control.Monad
import Control.Exception (assert)
import Data.Char (isSpace)

import qualified System.Console.Isocline as Iso
import Language.Haskell.Exts.SrcLoc

import Debug.Trace

format :: SymTable -> IO String
format = concatMapM (uncurry fmtFileResult) . toList

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

fmtFileResult :: FilePath -> [SrcSpanInfo] -> IO String
fmtFileResult path spans = do
    content <- lines <$> readFile path
    let 
        line l = content !! pred l
        fmtLine line l c1 c2 = lineNo <> start <> "[bgcolor=green]" <> hl <> "[/]" <> end
          where
            (start, rest) = splitAt (pred c1) line
            (hl, end) = splitAt (c2-c1) rest
            lineNo = "[orange]" <> show l <> ":[/]"
        fmtSpanResult (SrcSpanInfo (SrcSpan _ l1 c1 l2 c2) _) = assert (l1 == l2) $
            fmtLine (line l1) l1 c1 c2
    pure $ "[b green]" <> path <> "[/]\n" <> unlines (map fmtSpanResult spans)

formatLn :: SymTable -> IO ()
formatLn = printFormatted <=< format

putFmt "" = pure ()
putFmt x = Iso.putFmt x

printFormatted :: String -> IO ()
printFormatted = putFmt