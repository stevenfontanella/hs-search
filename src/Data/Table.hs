module Data.Table (
    Table
  , add
  , singleton
  , lookup
  , (!?)
) where

import Prelude hiding (lookup)

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

type Name = String
newtype Table n s = Table
 { toMap :: Map n [s]
 } deriving (Show)

add :: Ord n => n -> s -> Table n s -> Table n s
add name span = (<>) $ singleton name span

instance Ord n => Semigroup (Table n s) where
  Table a <> Table b = Table $ M.unionWith (++) a b

instance Ord n => Monoid (Table n s) where
  mempty = Table mempty

singleton :: n -> s -> Table n s
singleton name span = Table $ M.singleton name [span]

-- Nothing is conceptually the same as [] here;
-- we can't have a key with an empty list
lookup :: Ord n => n -> Table n s -> [s]
lookup n = fromMaybe [] . M.lookup n . toMap

(!?) :: Ord n => Table n s -> n -> [s]
(!?) = flip lookup
