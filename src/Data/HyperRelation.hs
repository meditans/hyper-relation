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

import           Data.Maybe  (catMaybes)
import Control.Applicative (liftA2)

import           Data.HyperRelation.Internal.IndexMapping (IndexMapping)
import qualified Data.HyperRelation.Internal.IndexMapping as IM

import Data.HyperRelation.Internal.ToRelation
import Data.HyperRelation.Internal.Proxy


-- PROVE

a = IM.insert 4 "due" $ IM.insert 3 "tre" $ IM.insert 2 "due" $ IM.empty


-- Parte sui tipi
data family ConcreteHR (as :: [*])
data instance ConcreteHR '[]       = EndHR
data instance ConcreteHR (a ': as) = IndexMapping a :<=>: ConcreteHR as

infixr 4 :<=>:

deriving instance Show (ConcreteHR '[])
deriving instance (Show a, Show (ConcreteHR as)) => Show (ConcreteHR (a ': as))

class HyperRelation (as :: [*]) where
  null :: ConcreteHR as -> Bool
  size :: ConcreteHR as -> Int
  empty :: ConcreteHR as
  lookupRelation :: Int -> ConcreteHR as -> Maybe (Relation as)
  singleton' :: Relation as -> ConcreteHR as
  insert' :: Relation as -> ConcreteHR as -> ConcreteHR as

instance HyperRelation '[] where
  null EndHR = True
  size EndHR = 0
  empty = EndHR
  lookupRelation _ EndHR = Just EndRel
  singleton' EndRel = EndHR
  insert' EndRel EndHR = EndHR

instance (Ord a, HyperRelation as) => HyperRelation (a ': as) where
  null (a :<=>: _) = IM.size a == 0
  size (a :<=>: _) = IM.size a
  empty = IM.empty :<=>: empty
  lookupRelation i (a :<=>: ims) = liftA2 (:<->:) (IM.lookupIndex i a) (lookupRelation i ims)
  singleton' (x :<->: xs) = IM.singleton x :<=>: singleton' xs
  insert' (x :<->: xs) (m :<=>: ms) = IM.insert (IM.size m + 1) x m :<=>: insert' xs ms

inserto :: (HyperRelation as, ToRelation a as) => a -> ConcreteHR as -> ConcreteHR as
inserto r m = insert' (toRelation r) m

provaInserto :: ConcreteHR '[Int, String]
provaInserto = inserto (1 :: Int, "uno" :: String)
             $ inserto (2 :: Int, "uno" :: String) empty

provaInserto2 :: ConcreteHR '[Int, String, Int]
provaInserto2 = inserto (1 :: Int, "uno" :: String, 2 :: Int)
              $ inserto (2 :: Int, "uno" :: String, 3 :: Int) empty

provaInserto3 :: ConcreteHR '[Int, String, Int]
provaInserto3 = inserto (1 :: Int, "uno" :: String, 2 :: Int)
              $ inserto (2 :: Int, "uno" :: String, 3 :: Int) empty

class HyperRelationLookup (n :: Nat) (as :: [*]) where
    -- |Is the key a member at the specified side of the polymap.
    member :: Proxy n -> TypeAt n as -> ConcreteHR as -> Bool

    -- |Lookup the /index/ of a key, which is its zero-based index in the storage
    -- at the specified side of the polymap. The index is a number from /0/ up
    -- to, but not including, the 'size' of the polymap.
    lookupIndices :: Proxy n -> TypeAt n as -> ConcreteHR as -> [Int]

instance HyperRelationLookup n '[] where
  member Proxy _ EndHR = False
  lookupIndices Proxy _ EndHR = []

instance Ord a => HyperRelationLookup 'Z (a ': as) where
  member Proxy a (m :<=>: _) = IM.elem a m
  lookupIndices Proxy a (m :<=>: _) = IM.lookup a m

instance (Ord a, HyperRelationLookup n as) => HyperRelationLookup ('S n) (a ': as) where
  member Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms
  lookupIndices Proxy x (_ :<=>: ms) = lookupIndices (Proxy :: Proxy n) x ms

lookupo :: (HyperRelation as, HyperRelationLookup n as) => Proxy n -> TypeAt n as -> ConcreteHR as -> [Relation as]
lookupo proxy x m = catMaybes $ map (\i -> lookupRelation i m) (lookupIndices proxy x m)
