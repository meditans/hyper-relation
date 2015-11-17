{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.HyperRelation
  ( Relation, toRelation, fromRelation
  , hyperLookup
  , lookup
  , fromList
  , assoc
  , HyperRelation (..)
  , Maybes (..)
  ) where

import           Prelude hiding      (lookup)
import           Control.Applicative (liftA2)
import           Data.Maybe          (catMaybes)
import           Data.Hashable       (Hashable)

import qualified Data.HyperRelation.Internal.IndexMapping as IM
import           Data.HyperRelation.Internal.Proxy
import           Data.HyperRelation.Internal.Relation

import qualified Data.HashSet as HS

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
  simultLookup   :: Relation (Maybes as) -> HyperRelation as -> Maybe (HS.HashSet Int)

instance HRC '[] where
  null EndHR             = True
  size EndHR             = 0
  empty                  = EndHR
  lookupRelation _ EndHR = Just EndRel
  singleton' EndRel      = EndHR
  insert' EndRel EndHR   = EndHR
  simultLookup EndRel EndHR = Nothing

instance (Hashable a, Eq a, HRC as) => HRC (a ': as) where
  null (a :<=>: _)                   = IM.size a == 0
  size (a :<=>: _)                   = IM.size a
  empty                              = IM.empty :<=>: empty
  lookupRelation i (a :<=>: ims)     = liftA2 (:<->:) (IM.lookupIndex i a) (lookupRelation i ims)
  singleton' (x :<->: xs)            = IM.singleton x :<=>: singleton' xs
  insert' (x :<->: xs) (m :<=>: ms)  = IM.insert (IM.size m + 1) x m :<=>: insert' xs ms

  simultLookup (Nothing :<->: as) (h :<=>: hs) = simultLookup as hs
  simultLookup (Just a :<->: as)  (h :<=>: hs) = case simultLookup as hs of
    Just hs -> Just (HS.intersection hs (IM.lookup a h))
    Nothing -> Just (IM.lookup a h)

inserto :: (HRC as, IsRelation a as) => a -> HyperRelation as -> HyperRelation as
inserto r m = insert' (toRelation r) m

fromList :: (HRC as, IsRelation a as) => [a] -> HyperRelation as
fromList = foldl (flip inserto) empty

class HRL (n :: Nat) (as :: [*]) where
    member        :: Proxy n -> TypeAt n as -> HyperRelation as -> Bool
    lookupIndices :: Proxy n -> TypeAt n as -> HyperRelation as -> HS.HashSet Int

instance HRL n '[] where
  member        Proxy _ EndHR = False
  lookupIndices Proxy _ EndHR = HS.empty

instance (Eq a, Hashable a) => HRL 'Z (a ': as) where
  member        Proxy a (m :<=>: _) = IM.elem a m
  lookupIndices Proxy a (m :<=>: _) = IM.lookup a m

instance (Eq a, Hashable a, HRL n as) => HRL ('S n) (a ': as) where
  member        Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms
  lookupIndices Proxy x (_ :<=>: ms) = lookupIndices (Proxy :: Proxy n) x ms

-- | Example usage: `lookup first rel hyrel` finds all the relations containing
--   `rel` in the first position.
lookup :: (HRL n as, HRC as, IsRelation a as) => Proxy n -> TypeAt n as -> HyperRelation as -> [a]
lookup proxy x m = map fromRelation . catMaybes . map (\i -> lookupRelation i m) . HS.toList $ (lookupIndices proxy x m)

hyperLookup :: (HRC as, IsRelation a (Maybes as), IsRelation b as) => a -> HyperRelation as -> [b]
hyperLookup rel hyrel = map fromRelation . catMaybes
                         $ map (\i -> lookupRelation i hyrel) . HS.toList . maybe HS.empty id
                         $ simultLookup (toRelation rel) hyrel

assoc :: (HRC as, IsRelation a as) => HyperRelation as -> [a]
assoc xs = map fromRelation . (maybe [] id) . sequence $ map (\i -> lookupRelation i xs) [1..size xs]

type family Maybes a where
  Maybes ('[])     = '[]
  Maybes (a ': as) = Maybe a ': Maybes as
