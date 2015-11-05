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

module Data.ConcreteHR where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (maybeToList, maybe, catMaybes)
import           Data.Set    (Set)
import qualified Data.Set    as S

import Control.Applicative (liftA2)

data IndexMapping a = IndexMapping
            { toIndices   :: Map a (Set Int)
            , fromIndices :: IntMap a
            } deriving (Show, Eq)

imEmpty :: IndexMapping a
imEmpty = IndexMapping (M.empty) (IM.empty)

imSingleton :: a -> IndexMapping a
imSingleton a = IndexMapping (M.singleton a $ S.singleton 1) (IM.singleton 0 a)

imInsert :: Ord a => Int -> a -> IndexMapping a -> IndexMapping a
imInsert i a (IndexMapping m im) =
  IndexMapping
    (M.insertWith S.union a (S.singleton i) m)
    (IM.insert i a im)

imLookup :: Ord a => a -> IndexMapping a -> [Int]
imLookup a (IndexMapping m im) = maybe [] S.toList (M.lookup a m)

-- lookupIndex :: Int -> IndexMapping a -> [a]
-- lookupIndex i (IndexMapping m im) = maybeToList (IM.lookup i im)

imLookupIndex :: Int -> IndexMapping a -> Maybe a
imLookupIndex i (IndexMapping m im) = IM.lookup i im

imSize :: IndexMapping a -> Int
imSize (IndexMapping m im) = IM.size im

imElem :: Ord a => a -> IndexMapping a -> Bool
imElem a (IndexMapping m im) = maybe False (const True) (M.lookup a m)

-- PROVE

a = imInsert 4 "due" $ imInsert 3 "tre" $ imInsert 2 "due" $ imEmpty


-- Parte sui tipi
data family ConcreteHR (as :: [*])
data instance ConcreteHR '[]       = EndHR
data instance ConcreteHR (a ': as) = IndexMapping a :<=>: ConcreteHR as

infixr 4 :<=>:

deriving instance Show (ConcreteHR '[])
deriving instance (Show a, Show (ConcreteHR as)) => Show (ConcreteHR (a ': as))

-- |A relation whose sides are defined by a list of types.
data family Relation (as :: [*])
data instance Relation '[] = EndRel
data instance Relation (a ': as) = a :<->: Relation as

type family TypeAt (n :: Nat) (as :: [*]) where
    TypeAt 'Z     (a ': as) = a
    TypeAt ('S n) (a ': as) = TypeAt n as

infixr 4 :<->:

deriving instance Show (Relation '[])
deriving instance (Show a, Show (Relation as)) => Show (Relation (a ': as))

class RelationSideAt (n :: Nat) (as :: [*]) where
    -- |Retrieve the value at the specified side of the relation.
    sideAt :: Proxy n -> Relation as -> TypeAt n as

instance RelationSideAt 'Z (a ': as) where
    sideAt Proxy (x :<->: _) = x

instance RelationSideAt n as => RelationSideAt ('S n) (a ': as) where
    sideAt Proxy (_ :<->: xs) = sideAt (Proxy :: Proxy n) xs


-- naturali
data Nat = Z | S Nat

data Proxy (a :: Nat) = Proxy

first :: Proxy Z
first = Proxy

second :: Proxy (S Z)
second = Proxy

third :: Proxy (S (S Z))
third = Proxy

fourth :: Proxy (S (S (S Z)))
fourth = Proxy

fifth :: Proxy (S (S (S (S Z))))
fifth = Proxy

sixth :: Proxy (S (S (S (S (S Z)))))
sixth = Proxy

seventh :: Proxy (S (S (S (S (S (S Z))))))
seventh = Proxy

eigthth :: Proxy (S (S (S (S (S (S (S Z)))))))
eigthth = Proxy

ninth :: Proxy (S (S (S (S (S (S (S (S Z))))))))
ninth = Proxy

tenth :: Proxy (S (S (S (S (S (S (S (S (S Z)))))))))
tenth = Proxy



-- lookupH :: (HyperRelationLookup n as) => Proxy n -> TypeAt n as -> ConcreteHR as -> Maybe (Relation as)
-- lookupH proxy x m = case lookupIndex proxy x m of
--     Nothing -> Nothing
--     Just i  -> lookupRelation i m

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
  null (a :<=>: _) = imSize a == 0
  size (a :<=>: _) = imSize a
  empty = imEmpty :<=>: empty
  lookupRelation i (a :<=>: ims) = liftA2 (:<->:) (imLookupIndex i a) (lookupRelation i ims)
  singleton' (x :<->: xs) = imSingleton x :<=>: singleton' xs
  insert' (x :<->: xs) (m :<=>: ms) = imInsert (imSize m + 1) x m :<=>: insert' xs ms

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

-- HyperRelationLookup
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
  member Proxy a (m :<=>: _) = imElem a m
  lookupIndices Proxy a (m :<=>: _) = imLookup a m

instance (Ord a, HyperRelationLookup n as) => HyperRelationLookup ('S n) (a ': as) where
  member Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms
  lookupIndices Proxy x (_ :<=>: ms) = lookupIndices (Proxy :: Proxy n) x ms

lookupo :: (HyperRelation as, HyperRelationLookup n as) => Proxy n -> TypeAt n as -> ConcreteHR as -> [Relation as]
lookupo proxy x m = catMaybes $ map (\i -> lookupRelation i m) (lookupIndices proxy x m)



-- TORELATION

class ToRelation a (as :: [*]) where
    toRelation :: a -> Relation as

instance ToRelation (Relation as) as where toRelation r = r

instance ToRelation () '[]  where
    toRelation () = EndRel

instance ToRelation a0 '[a0] where
    toRelation x0 = x0 :<->: EndRel

instance ToRelation (a0, a1) '[a0, a1] where
    toRelation (x0, x1) = x0 :<->: x1 :<->: EndRel

instance ToRelation (a0, a1, a2) '[a0, a1, a2] where
    toRelation (x0, x1, x2) = x0 :<->: x1 :<->: x2 :<->: EndRel

instance ToRelation (a0, a1, a2, a3) '[a0, a1, a2, a3] where
    toRelation (x0, x1, x2, x3) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: EndRel

instance ToRelation (a0, a1, a2, a3, a4) '[a0, a1, a2, a3, a4] where
    toRelation (x0, x1, x2, x3, x4) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: EndRel

instance ToRelation (a0, a1, a2, a3, a4, a5) '[a0, a1, a2, a3, a4, a5] where
    toRelation (x0, x1, x2, x3, x4, x5) = x0 :<->: x1 :<->: x2 :<->: x3
                                    :<->: x4 :<->: x5 :<->: EndRel

instance ToRelation (a0, a1, a2, a3, a4, a5, a6) '[a0, a1, a2, a3, a4, a5, a6] where
    toRelation (x0, x1, x2, x3, x4, x5, x6) = x0 :<->: x1 :<->: x2 :<->: x3
                                        :<->: x4 :<->: x5 :<->: x6 :<->: EndRel

instance ToRelation (a0, a1, a2, a3, a4, a5, a6, a7) '[a0, a1, a2, a3, a4, a5, a6, a7] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4
                                            :<->: x5 :<->: x6 :<->: x7 :<->: EndRel

instance ToRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8) '[a0, a1, a2, a3, a4, a5, a6, a7, a8] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5
                                                :<->: x6 :<->: x7 :<->: x8 :<->: EndRel

instance ToRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) '[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5
                                                    :<->: x6 :<->: x7 :<->: x8 :<->: x9 :<->: EndRel
