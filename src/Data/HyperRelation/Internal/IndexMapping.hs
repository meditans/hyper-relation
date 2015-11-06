-- | This module is meant to be imported qualified

module Data.HyperRelation.Internal.IndexMapping where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as S

-- Forse questi nomi interni sono da cambiare
data IndexMapping a = IndexMapping
            { toIndices   :: HashMap a (HashSet Int)
            , fromIndices :: HashMap Int a
            } deriving (Show, Eq)

empty :: IndexMapping a
empty = IndexMapping (M.empty) (M.empty)

singleton :: (Hashable a) => a -> IndexMapping a
singleton a = IndexMapping (M.singleton a $ S.singleton 1) (M.singleton 1 a)

insert :: (Hashable a, Eq a) => Int -> a -> IndexMapping a -> IndexMapping a
insert i a (IndexMapping m im) =
  IndexMapping
    (M.insertWith S.union a (S.singleton i) m)
    (M.insert i a im)

lookup :: (Hashable a, Eq a) => a -> IndexMapping a -> [Int]
lookup a (IndexMapping m im) = maybe [] S.toList (M.lookup a m)

lookupIndex :: (Hashable a) => Int -> IndexMapping a -> Maybe a
lookupIndex i (IndexMapping m im) = M.lookup i im

size :: (Hashable a) => IndexMapping a -> Int
size (IndexMapping m im) = M.size im

elem :: (Hashable a, Eq a) => a -> IndexMapping a -> Bool
elem a (IndexMapping m im) = maybe False (const True) (M.lookup a m)
