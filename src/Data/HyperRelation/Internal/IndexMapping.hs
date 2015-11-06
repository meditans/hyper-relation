-- | This module is meant to be imported qualified

module Data.HyperRelation.Internal.IndexMapping where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Set    (Set)
import qualified Data.Set    as S

data IndexMapping a = IndexMapping
            { toIndices   :: Map a (Set Int)
            , fromIndices :: IntMap a
            } deriving (Show, Eq)

empty :: IndexMapping a
empty = IndexMapping (M.empty) (IM.empty)

singleton :: a -> IndexMapping a
singleton a = IndexMapping (M.singleton a $ S.singleton 1) (IM.singleton 0 a)

insert :: Ord a => Int -> a -> IndexMapping a -> IndexMapping a
insert i a (IndexMapping m im) =
  IndexMapping
    (M.insertWith S.union a (S.singleton i) m)
    (IM.insert i a im)

lookup :: Ord a => a -> IndexMapping a -> [Int]
lookup a (IndexMapping m im) = maybe [] S.toList (M.lookup a m)

lookupIndex :: Int -> IndexMapping a -> Maybe a
lookupIndex i (IndexMapping m im) = IM.lookup i im

size :: IndexMapping a -> Int
size (IndexMapping m im) = IM.size im

elem :: Ord a => a -> IndexMapping a -> Bool
elem a (IndexMapping m im) = maybe False (const True) (M.lookup a m)
