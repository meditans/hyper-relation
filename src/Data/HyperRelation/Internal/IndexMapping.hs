-- | This module is meant to be imported qualified
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Data.HyperRelation.Internal.IndexMapping
       ( IndexMapping (..)
       , empty
       , singleton
       , insert
       , lookup
       , lookupIndex
       , size
       , elem
       , deleteIndex
       , Hashable
       ) where

import           Prelude hiding (lookup, elem)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as S

-- Forse questi nomi interni sono da cambiare

data IndexMapping a = IndexMapping
                      { toIndices :: HashMap a (HashSet Int)
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

lookup :: (Hashable a, Eq a) => a -> IndexMapping a -> HashSet Int
lookup a (IndexMapping m im) = maybe (S.empty) id (M.lookup a m)

lookupIndex :: (Hashable a) => Int -> IndexMapping a -> Maybe a
lookupIndex i (IndexMapping m im) = M.lookup i im

size :: (Hashable a) => IndexMapping a -> Int
size (IndexMapping m im) = M.size im

-- riscrivere con la funzione nativa
elem :: (Hashable a, Eq a) => a -> IndexMapping a -> Bool
elem a (IndexMapping m im) = maybe False (const True) (M.lookup a m)

deleteIndex :: (Hashable a, Eq a) => Int -> IndexMapping a -> IndexMapping a
deleteIndex i (IndexMapping m im) = case lookupIndex i (IndexMapping m im) of
    Just a  -> IndexMapping (deleteOrRemove a i m) (M.delete i im)
    Nothing -> IndexMapping m im

----------------- Not exported functions ----------------

deleteOrRemove :: (Eq a, Hashable a) => a -> Int -> HashMap a (HashSet Int) -> HashMap a (HashSet Int)
deleteOrRemove a i m = if S.size (M.lookupDefault S.empty a m) > 1
                       then M.adjust (S.delete i) a m
                       else M.delete a m

