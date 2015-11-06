{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.HyperRelation.Internal.Relation where

import Data.HyperRelation.Internal.Proxy

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

class ToRelation a (as :: [*]) where
    toRelation :: a -> Relation as

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
