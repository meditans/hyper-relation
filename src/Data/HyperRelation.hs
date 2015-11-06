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

module Data.HRC where

import           Control.Applicative (liftA2)
import           Data.Maybe          (catMaybes)

import qualified Data.HyperRelation.Internal.IndexMapping as IM
import           Data.HyperRelation.Internal.Proxy
import           Data.HyperRelation.Internal.Relation


-- PROVE

a = IM.insert 4 "due" $ IM.insert 3 "tre" $ IM.insert 2 "due" $ IM.empty


-- Parte sui tipi
data family HyperRelation (as :: [*])
data instance HyperRelation '[]       = EndHR
data instance HyperRelation (a ': as) = IM.IndexMapping a :<=>: HyperRelation as

infixr 4 :<=>:

deriving instance Show (HyperRelation '[])
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

instance (Ord a, HRC as) => HRC (a ': as) where
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
    member :: Proxy n -> TypeAt n as -> HyperRelation as -> Bool
    lookupIndices :: Proxy n -> TypeAt n as -> HyperRelation as -> [Int]

instance HRL n '[] where
  member Proxy _ EndHR = False
  lookupIndices Proxy _ EndHR = []

instance Ord a => HRL 'Z (a ': as) where
  member Proxy a (m :<=>: _) = IM.elem a m
  lookupIndices Proxy a (m :<=>: _) = IM.lookup a m

instance (Ord a, HRL n as) => HRL ('S n) (a ': as) where
  member Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms
  lookupIndices Proxy x (_ :<=>: ms) = lookupIndices (Proxy :: Proxy n) x ms

lookupo :: (HRC as, HRL n as) => Proxy n -> TypeAt n as -> HyperRelation as -> [Relation as]
lookupo proxy x m = catMaybes $ map (\i -> lookupRelation i m) (lookupIndices proxy x m)
