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

import           Control.Applicative                      (liftA2)
import           Data.Hashable                            (Hashable)
import           Data.Maybe                               (catMaybes, fromJust, fromMaybe)
import           Prelude                                  hiding (lookup)

import qualified Data.HyperRelation.Internal.IndexMapping as IMM
import           Data.HyperRelation.Internal.Proxy
import           Data.HyperRelation.Internal.Relation

import qualified Data.HashMap.Strict                      as HM
import qualified Data.HashSet                             as HS
import           Data.IntMap                              (IntMap)
import qualified Data.IntMap                              as IM
import qualified Data.IntSet                              as IS

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

lookupOUT :: (IsRelation t (Maybes as), HRC as) => t -> HyperRelation as -> [Int]
lookupOUT t (HyperRelation itd dti) = fromMaybe [] . fmap IS.toList $ simultLookupIN (toRelation t) dti

lookupOUT2 :: (IsRelation t (Maybes as), HRC as, IsRelation s as) => t -> HyperRelation as -> [s]
lookupOUT2 t (HyperRelation itd dti) = map (fromRelation . fromJust . flip IM.lookup itd) . fromMaybe [] . fmap IS.toList $ simultLookupIN (toRelation t) dti

class HRC (as :: [*]) where
  emptyIN     :: HyperRelation' as
  singletonIN :: Relation as -> HyperRelation' as
  insertIN    :: Int -> Relation as -> HyperRelation' as -> HyperRelation' as
  simultLookupIN :: Relation (Maybes as) -> HyperRelation' as -> Maybe IS.IntSet

instance HRC '[] where
  emptyIN                     = EndHR
  singletonIN EndRel          = EndHR
  insertIN _ EndRel EndHR     = EndHR
  simultLookupIN EndRel EndHR = Nothing

instance (Hashable a, Eq a, HRC as) => HRC (a ': as) where
  emptyIN = HM.empty :<=>: emptyIN
  singletonIN (x :<->: xs) = HM.singleton x (IS.singleton 0) :<=>: singletonIN xs
  insertIN i (x :<->: xs) (m :<=>: ms) = HM.insertWith IS.union x (IS.singleton i) m :<=>: insertIN i xs ms

  -- | Probabilmente devo usare la monade First
  -- Provare inoltre cosa fa quando non c'e' quello che si sta cercando.
  simultLookupIN (Nothing :<->: as) (h :<=>: hs) = simultLookupIN as hs
  simultLookupIN (Just a  :<->: as) (h :<=>: hs) =
    case simultLookupIN as hs of
      Just goodIndices -> case HM.lookup a h of
        Just newIndices -> Just $ IS.intersection goodIndices newIndices
        Nothing         -> Just goodIndices
      Nothing -> case HM.lookup a h of
        Just newIndices -> Just newIndices
        Nothing         -> Nothing

-- se mi vengono ritornati indici, vuole dire che devo rastremare, in altre
-- parole il nothing vuol dire che non me ne frega niente, e ce l'ho solo se non
-- me ne frega niente in nessuno dei due posti.

fromList :: (HRC as, IsRelation a as) => [a] -> HyperRelation as
fromList = foldl (flip insertOUT) emptyOUT


-- -- | Example usage: `lookup first rel hyrel` finds all the relations containing
-- --   `rel` in the first position.
-- lookup :: (HRL n as, HRC as, IsRelation a as) => Proxy n -> TypeAt n as -> HyperRelation' as -> [a]
-- lookup proxy x m = map fromRelation . catMaybes . map (\i -> lookupRelation i m) . HS.toList $ (lookupIndices proxy x m)

-- hyperLookup :: (HRC as, IsRelation a (Maybes as), IsRelation b as) => a -> HyperRelation' as -> [b]
-- hyperLookup rel hyrel = map fromRelation . catMaybes
--                          $ map (\i -> lookupRelation i hyrel) . HS.toList . maybe HS.empty id
--                          $ simultLookup (toRelation rel) hyrel

-- assoc :: (HRC as, IsRelation a as) => HyperRelation' as -> [a]
-- assoc xs = map fromRelation . (maybe [] id) . sequence $ map (\i -> lookupRelation i xs) [1..size xs]

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
