module Data.SymTable (
    module Data.Table
  , SymTable
) where

import Data.Table
import Language.Haskell.Exts.SrcLoc

type SymTable = Table FilePath SrcSpanInfo
