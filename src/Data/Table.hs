module Data.Table (
    Table
  , add
  , singleton
  , lookup
  , (!?)
  , toList
) where

import Prelude hiding (lookup)

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

newtype Table k m = Table
 { toMap :: Map k m
 } deriving (Show, Eq)

add :: (Ord k, Monoid m) => k -> m -> Table k m -> Table k m
add name span = (<>) $ singleton name span

instance (Ord k, Monoid m) => Semigroup (Table k m) where
  Table a <> Table b = Table $ M.unionWith (<>) a b

instance (Ord k, Monoid m) => Monoid (Table k m) where
  mempty = Table mempty

singleton :: k -> m -> Table k m
singleton name span = Table $ M.singleton name span

-- Nothing is conceptually the same as [] here;
-- we can't have a key with an empty list
lookup :: (Ord k, Monoid m) => k -> Table k m -> m
lookup n = fromMaybe mempty . M.lookup n . toMap

(!?) :: (Ord k, Monoid m) => Table k m -> k -> m
(!?) = flip lookup

-- toList :: Table n s -> [(n, s)]
-- toList = concatMap (\(k, vs) -> fmap (k,) vs) . M.toList . toMap

toList :: Table k m -> [(k, m)]
toList = M.toList . toMap
