{-# LANGUAGE KindSignatures, DataKinds #-}

module Data.HyperRelation.Internal.Proxy where

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
