{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeOperators          #-}

module Data.HyperRelation.Internal.Relation where

import           Data.Functor.Identity

data Relation :: (* -> *) -> [*] -> * where
  EndR    :: Relation f '[]
  (:<->:) :: f a -> Relation f as -> Relation f (a ': as)

infixr 4 :<->:

instance Show (Relation f '[]) where
  show EndR = "EndR"
instance (Show (f a), Show (Relation f as)) => Show (Relation f (a ': as)) where
  show (a :<->: as) = show a ++ " :<->: " ++ show as

-- ---------- RelationSideAt class
-- class RelationSideAt (n :: Nat) (as :: [*]) where
--     -- | Retrieve the value at the specified side of the relation.
--     sideAt :: Proxy n -> Relation as -> TypeAt n as

-- instance RelationSideAt 'Z (a ': as) where
--     sideAt Proxy (x :<->: _) = x

-- instance RelationSideAt n as => RelationSideAt ('S n) (a ': as) where
--     sideAt Proxy (_ :<->: xs) = sideAt (Proxy :: Proxy n) xs

exRel :: Relation Identity '[Integer, Char]
exRel = Identity 3 :<->: Identity 'c' :<->: EndR

justify :: Relation Identity as -> Relation Maybe as
justify EndR                  = EndR
justify (Identity a :<->: as) = Just a :<->: justify as

nullify :: Int -> Relation Maybe as -> Relation Maybe as
nullify _ EndR = EndR
nullify 1 (fa :<->: fas) = Nothing :<->: fas
nullify n (fa :<->: fas) = fa :<->: nullify (n-1) fas


class IsRelation a as | a -> as, as -> a where
    toRelation   :: a -> Relation Identity as
    fromRelation :: Relation Identity as -> a

instance IsRelation () '[] where
  toRelation () = EndR
  fromRelation EndR = ()

instance IsRelation (a0, a1) '[a0, a1] where
    toRelation (x0, x1) = Identity x0 :<->: Identity x1 :<->: EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: EndR) = (x0, x1)

instance IsRelation (a0, a1, a2) '[a0, a1, a2] where
    toRelation (x0, x1, x2) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->: EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  EndR) =
      (x0, x1, x2)

instance IsRelation (a0, a1, a2, a3) '[a0, a1, a2, a3] where
    toRelation (x0, x1, x2, x3) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
      Identity x3 :<->: EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  Identity x3 :<->: EndR) =
      (x0, x1, x2, x3)

instance IsRelation (a0, a1, a2, a3, a4) '[a0, a1, a2, a3, a4] where
    toRelation (x0, x1, x2, x3, x4) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
      Identity x3 :<->: Identity x4 :<->: EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  Identity x3 :<->: Identity x4 :<->: EndR) =
      (x0, x1, x2, x3, x4)

instance IsRelation (a0, a1, a2, a3, a4, a5)
                   '[a0, a1, a2, a3, a4, a5] where
    toRelation (x0, x1, x2, x3, x4, x5) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
      Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
      EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
                  EndR) =
      (x0, x1, x2, x3, x4, x5)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6)
                   '[a0, a1, a2, a3, a4, a5, a6] where
    toRelation (x0, x1, x2, x3, x4, x5, x6) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
      Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
      Identity x6 :<->: EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
                  Identity x6 :<->: EndR) =
      (x0, x1, x2, x3, x4, x5, x6)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6, a7)
                   '[a0, a1, a2, a3, a4, a5, a6, a7] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
      Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
      Identity x6 :<->: Identity x7 :<->: EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
                  Identity x6 :<->: Identity x7 :<->: EndR) =
      (x0, x1, x2, x3, x4, x5, x6, x7)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8)
                   '[a0, a1, a2, a3, a4, a5, a6, a7, a8] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
      Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
      Identity x6 :<->: Identity x7 :<->: Identity x8 :<->:
      EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
                  Identity x6 :<->: Identity x7 :<->: Identity x8 :<->:
                  EndR) =
      (x0, x1, x2, x3, x4, x5, x6, x7, x8)

instance IsRelation (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
                   '[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] where
    toRelation (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) =
      Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
      Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
      Identity x6 :<->: Identity x7 :<->: Identity x8 :<->:
      Identity x9 :<->: EndR
    fromRelation (Identity x0 :<->: Identity x1 :<->: Identity x2 :<->:
                  Identity x3 :<->: Identity x4 :<->: Identity x5 :<->:
                  Identity x6 :<->: Identity x7 :<->: Identity x8 :<->:
                  Identity x9 :<->: EndR) =
      (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
