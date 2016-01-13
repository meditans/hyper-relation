{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Data.HyperRelation
  -- ( Relation, toRelation, fromRelation
  -- , hyperLookup
  -- , lookup
  -- , fromList
  -- , assoc
  -- , HyperRelation (..)
  -- , HyperRelation' (..)
  -- , Maybes (..)
  -- )
  where

import           Control.Applicative                  (liftA2)
import           Data.Hashable                        (Hashable)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as HS
import           Data.IntMap                          (IntMap)
import qualified Data.IntMap                          as IM
import qualified Data.IntSet                          as IS
import           Data.List                            (foldl', maximumBy, minimumBy)
import           Data.Maybe                           (catMaybes, fromJust, fromMaybe)
import           Data.Ord                             (comparing)
import           Prelude                              hiding (lookup)

-- import           Data.HyperRelation.Internal.Proxy
import           Data.HyperRelation.Internal.Relation

-- | A hyperrelation is composed of: a map:
--   Index -> (tuple of values)
--   TypeMap Index -> Type
--   Some accessory data
-- Maybe IsRelation is not the best name for this.

data HyperRelation (as :: [*]) = HyperRelation
                                 { indexToData :: IntMap (Relation as)
                                 , dataToIndex :: HyperRelation' as
                                 }

deriving instance (Show (Relation as), Show (HyperRelation' as)) => Show (HyperRelation as)
deriving instance (Eq   (Relation as), Eq   (HyperRelation' as)) => Eq   (HyperRelation as)

infixr 4 :<=>:

data family   HyperRelation' (as :: [*])
data instance HyperRelation' '[]       = EndHR
data instance HyperRelation' (a ': as) = HM.HashMap a IS.IntSet :<=>: HyperRelation' as

deriving instance                                       Show (HyperRelation' '[])
deriving instance (Show a, Show (HyperRelation' as)) => Show (HyperRelation' (a ': as))

deriving instance                                   Eq (HyperRelation' '[])
deriving instance (Eq a, Eq (HyperRelation' as)) => Eq (HyperRelation' (a ': as))

nullOUT :: HyperRelation as -> Bool
nullOUT (HyperRelation itd _) = IM.null itd

sizeOUT :: HyperRelation as -> Int
sizeOUT (HyperRelation itd _) = IM.size itd

emptyOUT :: HRC as => HyperRelation as
emptyOUT = HyperRelation IM.empty emptyIN

singletonOUT :: (IsRelation t as, HRC as) => t  -> HyperRelation as
singletonOUT t = HyperRelation (IM.fromList [(0, toRelation t)]) (singletonIN (toRelation t))

insertOUT :: (IsRelation t as, HRC as) => t -> HyperRelation as -> HyperRelation as
insertOUT t (HyperRelation itd dti) = let ni = IM.size itd + 1
                                      in HyperRelation (IM.insert ni (toRelation t) itd)
                                                       (insertIN ni (toRelation t) dti)

lookupOUT :: (IsRelation t (Maybes as), HRC as, IsRelation s as) => t -> HyperRelation as -> [s]
lookupOUT t (HyperRelation itd dti) = map (fromRelation . fromJust . flip IM.lookup itd)
                                    . fromMaybe (IM.keys itd)
                                    . fmap IS.toList
                                    $ lookupIN (toRelation t) dti

class HRC (as :: [*]) where
  emptyIN     :: HyperRelation' as
  singletonIN :: Relation as -> HyperRelation' as
  insertIN    :: Int -> Relation as -> HyperRelation' as -> HyperRelation' as
  lookupIN :: Relation (Maybes as) -> HyperRelation' as -> Maybe IS.IntSet

instance HRC '[] where
  emptyIN                     = EndHR
  singletonIN EndRel          = EndHR
  insertIN _ EndRel EndHR     = EndHR
  lookupIN EndRel EndHR = Nothing

instance (Hashable a, Eq a, HRC as) => HRC (a ': as) where
  emptyIN = HM.empty :<=>: emptyIN
  singletonIN (x :<->: xs) = HM.singleton x (IS.singleton 0) :<=>: singletonIN xs
  insertIN i (x :<->: xs) (m :<=>: ms) = HM.insertWith IS.union x (IS.singleton i) m :<=>: insertIN i xs ms
  lookupIN (Nothing :<->: as) (h :<=>: hs) = lookupIN as hs
  lookupIN (Just a  :<->: as) (h :<=>: hs) =
    case lookupIN as hs of
      Just is -> Just $ maybe (IS.empty) (IS.intersection is) (HM.lookup a h)
      Nothing -> Just $ maybe (IS.empty) id                   (HM.lookup a h)

fromList :: (HRC as, IsRelation a as) => [a] -> HyperRelation as
fromList = foldl (flip insertOUT) emptyOUT

--------------------------------------------------------------------------------
-- Example section
--------------------------------------------------------------------------------

exHyperRelation :: HyperRelation '[String, Int, Char]
exHyperRelation = HyperRelation (IM.fromList [(1, ("carlo" :<->: 25 :<->: 'n' :<->: EndRel))])
                                ( HM.fromList [("carlo" , IS.fromList [1])]
                            :<=>: HM.fromList [(25      , IS.fromList [1])]
                            :<=>: HM.fromList [('n'     , IS.fromList [1])]
                            :<=>: EndHR)

-- demonstrating fromList
exHyperRelation2 :: HyperRelation '[String, Int, Char]
exHyperRelation2 = fromList [ ("carlo"  , 25, 'c')
                            , ("valerio", 21, 'v')
                            ]

-- Sarebbe bello avere un combinatore generico per le iperrelazioni, ovvero data
-- una funzione per estrarre qualcosa dalla prima mappa e una serie di funzioni
-- per estrarre le cose dalla seconda, restituisce un risultato compiuto.
