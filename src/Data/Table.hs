module Data.Table (
    Table
  , addName
  , singleton
) where

import Data.Map (Map)
import qualified Data.Map as M

type Name = String
newtype Table n s = Table
 { toMap :: Map n [s]
 } deriving (Show)

addName :: Ord n => n -> s -> Table n s -> Table n s
addName name span = (<>) $ singleton name span

instance Ord n => Semigroup (Table n s) where
  Table a <> Table b = Table $ M.unionWith (++) a b

instance Ord n => Monoid (Table n s) where
  mempty = Table mempty

singleton :: n -> s -> Table n s
singleton name span = Table $ M.singleton name [span]
