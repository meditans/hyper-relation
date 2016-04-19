{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies, ScopedTypeVariables, TypeOperators, DataKinds #-}

module Data.HyperRelation.Internal.Relation where

import Data.HyperRelation.Internal.Proxy

---------- Relation data family
data family Relation (as :: [*])
data instance Relation '[] = EndRel
data instance Relation (a ': as) = a :<->: Relation as

type family TypeAt (n :: Nat) (as :: [*]) where
    TypeAt 'Z     (a ': as) = a
    TypeAt ('S n) (a ': as) = TypeAt n as

infixr 4 :<->:

deriving instance Show (Relation '[])
deriving instance (Show a, Show (Relation as)) => Show (Relation (a ': as))

deriving instance Eq (Relation '[])
deriving instance (Eq a, Eq (Relation as)) => Eq (Relation (a ': as))

---------- Maybes type family
type family Maybes a where
  Maybes ('[])     = '[]
  Maybes (a ': as) = Maybe a ': Maybes as

---------- RelationSideAt class
class RelationSideAt (n :: Nat) (as :: [*]) where
    -- | Retrieve the value at the specified side of the relation.
    sideAt :: Proxy n -> Relation as -> TypeAt n as

instance RelationSideAt 'Z (a ': as) where
    sideAt Proxy (x :<->: _) = x

instance RelationSideAt n as => RelationSideAt ('S n) (a ': as) where
    sideAt Proxy (_ :<->: xs) = sideAt (Proxy :: Proxy n) xs

---------- IsRelation class
class IsRelation a as | a -> as, as -> a where
    toRelation   :: a -> Relation as
    fromRelation :: Relation as -> a

instance IsRelation () '[] where
  toRelation () = EndRel
  fromRelation EndRel = ()

instance IsRelation (a0, a1) '[a0, a1] where
    toRelation (x0, x1) = x0 :<->: x1 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: EndRel) = (x0, x1)

instance IsRelation (a0, a1, a2) '[a0, a1, a2] where
    toRelation (x0, x1, x2) = x0 :<->: x1 :<->: x2 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: EndRel) = (x0, x1, x2)

instance IsRelation (a0, a1, a2, a3) '[a0, a1, a2, a3] where
    toRelation (x0, x1, x2, x3) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: x3 :<->: EndRel) = (x0, x1, x2, x3)

instance IsRelation (a0, a1, a2, a3, a4) '[a0, a1, a2, a3, a4] where
    toRelation (x0, x1, x2, x3, x4) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: EndRel) = (x0, x1, x2, x3, x4)

instance IsRelation (a0, a1, a2, a3, a4, a5) '[a0, a1, a2, a3, a4, a5] where
    toRelation (x0, x1, x2, x3, x4, x5) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: EndRel) = (x0, x1, x2, x3, x4, x5)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6) '[a0, a1, a2, a3, a4, a5, a6] where
    toRelation (x0, x1, x2, x3, x4, x5, x6) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: EndRel) = (x0, x1, x2, x3, x4, x5, x6)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6, a7) '[a0, a1, a2, a3, a4, a5, a6, a7] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: EndRel) = (x0, x1, x2, x3, x4, x5, x6, x7)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8) '[a0, a1, a2, a3, a4, a5, a6, a7, a8] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: x8 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: x8 :<->: EndRel) = (x0, x1, x2, x3, x4, x5, x6, x7, x8)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) '[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: x8 :<->: x9 :<->: EndRel
    fromRelation (x0 :<->: x1 :<->: x2 :<->: x3 :<->: x4 :<->: x5 :<->: x6 :<->: x7 :<->: x8 :<->: x9 :<->: EndRel) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)


---- Examples
exRel :: Relation '[Integer, Char]
exRel = 3 :<->: 'c' :<->: EndRel

---------- Justify Class
class Justify (as :: [*]) where
    justify :: Relation as -> Relation (Maybes as)

instance Justify '[] where
    justify = id

instance Justify as => Justify (a ': as) where
    justify (a :<->: rs) = Just a :<->: justify rs

---------- Nullify class
class Nullify (as :: [*]) where
    nullify :: Int -> Relation (Maybes as) -> Relation (Maybes as)

instance Nullify '[] where
    nullify _ = id

instance (Justify as, Nullify as) => Nullify (a ': as) where
    nullify 1 (a :<->: rs) = Nothing :<->: rs
    -- nullify n (a :<->: rs) = a       :<->: nullify (n-1) rs
    nullify n (a :<->: rs) = undefined
