{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Data.HyperRelation where

import           Control.Applicative (liftA2)
import           Data.Maybe          (catMaybes)
import Data.Hashable (Hashable)

import qualified Data.HyperRelation.Internal.IndexMapping as IM
import           Data.HyperRelation.Internal.Proxy
import           Data.HyperRelation.Internal.Relation


-- PROVE

a :: IM.IndexMapping String
a = IM.insert 4 "due" $ IM.insert 3 "tre" $ IM.insert 2 "due" $ IM.empty


-- Parte sui tipi
data family   HyperRelation (as :: [*])
data instance HyperRelation '[]       = EndHR
data instance HyperRelation (a ': as) = IM.IndexMapping a :<=>: HyperRelation as

infixr 4 :<=>:

deriving instance                                      Show (HyperRelation '[])
deriving instance (Show a, Show (HyperRelation as)) => Show (HyperRelation (a ': as))

class HRC (as :: [*]) where
  null           :: HyperRelation as -> Bool
  size           :: HyperRelation as -> Int
  empty          :: HyperRelation as
  lookupRelation :: Int -> HyperRelation as -> Maybe (Relation as)
  singleton'     :: Relation as -> HyperRelation as
  insert'        :: Relation as -> HyperRelation as -> HyperRelation as

instance HRC '[] where
  null EndHR             = True
  size EndHR             = 0
  empty                  = EndHR
  lookupRelation _ EndHR = Just EndRel
  singleton' EndRel      = EndHR
  insert' EndRel EndHR   = EndHR

instance (Hashable a, Eq a, HRC as) => HRC (a ': as) where
  null (a :<=>: _)                   = IM.size a == 0
  size (a :<=>: _)                   = IM.size a
  empty                              = IM.empty :<=>: empty
  lookupRelation i (a :<=>: ims)     = liftA2 (:<->:) (IM.lookupIndex i a) (lookupRelation i ims)
  singleton' (x :<->: xs)            = IM.singleton x :<=>: singleton' xs
  insert' (x :<->: xs) (m :<=>: ms)  = IM.insert (IM.size m + 1) x m :<=>: insert' xs ms

inserto :: (HRC as, IsRelation a as) => a -> HyperRelation as -> HyperRelation as
inserto r m = insert' (toRelation r) m

fromListo :: (HRC as, IsRelation a as) => [a] -> HyperRelation as
fromListo = foldl (flip inserto) empty

provaInserto :: HyperRelation '[Int, String]
provaInserto = inserto (1 :: Int, "uno" :: String)
             $ inserto (2 :: Int, "uno" :: String) empty

provaInserto2 :: HyperRelation '[Int, String, Int]
provaInserto2 = inserto (1 :: Int, "uno" :: String, 2 :: Int)
              $ inserto (2 :: Int, "uno" :: String, 3 :: Int) empty

provaInserto3 :: HyperRelation '[Int, String, Int]
provaInserto3 = inserto (1 :: Int, "uno" :: String, 2 :: Int)
              $ inserto (2 :: Int, "uno" :: String, 3 :: Int) empty

provaInserto4 :: HyperRelation '[Int, String, Int]
provaInserto4 = fromListo ([(1, "uno", 2) , (2, "uno", 3)] :: [(Int, String, Int)])

class HRL (n :: Nat) (as :: [*]) where
    member        :: Proxy n -> TypeAt n as -> HyperRelation as -> Bool
    lookupIndices :: Proxy n -> TypeAt n as -> HyperRelation as -> [Int]

instance HRL n '[] where
  member        Proxy _ EndHR = False
  lookupIndices Proxy _ EndHR = []

instance (Eq a, Hashable a) => HRL 'Z (a ': as) where
  member        Proxy a (m :<=>: _) = IM.elem a m
  lookupIndices Proxy a (m :<=>: _) = IM.lookup a m

instance (Eq a, Hashable a, HRL n as) => HRL ('S n) (a ': as) where
  member        Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms
  lookupIndices Proxy x (_ :<=>: ms) = lookupIndices (Proxy :: Proxy n) x ms

lookupo :: (HRC as, HRL n as) => Proxy n -> TypeAt n as -> HyperRelation as -> [Relation as]
lookupo proxy x m = catMaybes $ map (\i -> lookupRelation i m) (lookupIndices proxy x m)

assoco :: (HRC as, IsRelation a as) => HyperRelation as -> [a]
assoco xs = map fromRelation . (maybe [] id) . sequence $ map (\i -> lookupRelation i xs) [1..size xs]

-- Interfaccia aggiuntiva:
{-
    (!) :: Questo e' per cercare l'indice di una relazione. Di fatto e' lookupRelation
    (\\) :: Questo e' per cancellare delle relazioni TODO

    , member :: Cioe' se una relazione e' presente, per intero? Questo e' interessante! TODO
    , notMember :: TODO
    , M.lookup :: Tutta la relazione
    , findWithDefault :: TODO

    , insertWith :: Inserisce una relazione decidendo come modificare TODO
    , insertWithKey :: Anche con la key della relazione TODO
    , insertLookupWithKey :: TODO

    -- ** Delete\/Update ----------> PENSARE AL RESTO DELLE FUNZIONALITA'
    , delete :: ASSOLUTAMENTE TODO
    , adjust :: ???
    , adjustWithKey :: ???
    , update :: ???
    , updateWithKey :: ???
    , updateLookupWithKey :: ???
    , alter :: ???

    -- * Combine

    -- ** Union -------------> COME UNIRE IN GENERALE?
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Universal combining function
    , mergeWithKey

    -- * Traversal
    -- ** Map
    , M.map
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , M.foldr
    , M.foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems :: Cosa sono in questo caso gli elementi di una mappa?
    , keys :: Cosa sono in questo caso le chiavi di una mappa?
    , assocs :: questa e' buona, ritornarle come una lista! Questo e' in ascending key order
    , keysSet
    , fromSet

    -- ** Lists --> Tutte queste sono interessanti
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered lists --> Quali ordinamenti
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter ---> Sono importanti
    , M.filter
    , filterWithKey
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey
-}
