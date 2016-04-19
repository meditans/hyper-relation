-- * Hyper-Relation

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
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
import           Data.Functor.Identity
import           Data.Hashable                        (Hashable)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as HS
import           Data.IntMap                          (IntMap)
import qualified Data.IntMap                          as IM
import qualified Data.IntSet                          as IS
import           Data.List                            (foldl', maximumBy,
                                                       minimumBy)
import           Data.Maybe                           (catMaybes, fromJust,
                                                       fromMaybe)
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
                                 { indexToData :: IntMap (Relation Identity as)
                                 , dataToIndex :: HyperRelation' as
                                 }

infixr 4 :<=>:

data HyperRelation' :: [*] -> * where
  EndHR :: HyperRelation' '[]
  (:<=>:) :: HM.HashMap a IS.IntSet -> HyperRelation' as -> HyperRelation' (a ': as)

instance Show (HyperRelation' '[]) where
  show EndHR = "EndHR"

instance (Show a, Show (HyperRelation' as)) => Show (HyperRelation' (a ': as)) where
  show (a :<=>: as) = show a ++ " :<=>: " ++ show as

nullOUT :: HyperRelation as -> Bool
nullOUT = IM.null . indexToData

sizeOUT :: HyperRelation as -> Int
sizeOUT = IM.size . indexToData

emptyOUT :: HRC as => HyperRelation as
emptyOUT = HyperRelation IM.empty emptyIN

singletonOUT :: (IsRelation Identity t as, HRC as) => t -> HyperRelation as
singletonOUT t = HyperRelation (IM.fromList [(0, toRelation t)])
                               (singletonIN (toRelation t))

insertOUT :: (IsRelation Identity t as, HRC as) => t -> HyperRelation as -> HyperRelation as
insertOUT t hr = HyperRelation (IM.insert ni (toRelation t) itd)
                               (insertIN  ni (toRelation t) dti)
  where itd = indexToData hr
        dti = dataToIndex hr
        ni = IM.size itd + 1

lookupOUT :: (HRC as, IsRelation Maybe t as, IsRelation Identity s as) => t -> HyperRelation as -> [s]
lookupOUT t hr = map (fromRelation . fromJust . flip IM.lookup itd)
               $ fromMaybe (IM.keys itd) (IS.toList <$> lookupIN (toRelation t) dti)
  where itd = indexToData hr
        dti = dataToIndex hr

class HRC (as :: [*]) where
  emptyIN     :: HyperRelation' as
  singletonIN :: Relation Identity as -> HyperRelation' as
  insertIN    :: Int -> Relation Identity as -> HyperRelation' as -> HyperRelation' as
  lookupIN    :: Relation Maybe as -> HyperRelation' as -> Maybe IS.IntSet

instance HRC '[] where
  emptyIN                = EndHR
  singletonIN EndR       = EndHR
  insertIN _  EndR EndHR = EndHR
  lookupIN    EndR EndHR = Nothing

instance (Hashable a, Eq a, HRC as) => HRC (a ': as) where
  emptyIN = HM.empty :<=>: emptyIN
  singletonIN (Identity x :<->: xs) = HM.singleton x (IS.singleton 0) :<=>: singletonIN xs
  insertIN i (Identity x :<->: xs) (m :<=>: ms) =
    HM.insertWith IS.union x (IS.singleton i) m :<=>: insertIN i xs ms
  lookupIN (Nothing :<->: as) (h :<=>: hs) = lookupIN as hs
  lookupIN (Just a  :<->: as) (h :<=>: hs) =
    case lookupIN as hs of
      Just is -> Just $ maybe (IS.empty) (IS.intersection is) (HM.lookup a h)
      Nothing -> Just $ maybe (IS.empty) id                   (HM.lookup a h)

fromList :: (HRC as, IsRelation Identity a as) => [a] -> HyperRelation as
fromList = foldl' (flip insertOUT) emptyOUT

union :: (HRC as, IsRelation Identity t as) => HyperRelation as -> HyperRelation as -> HyperRelation as
union hr1 hr2 = foldl' (flip insertOUT) hrMax (map fromRelation . IM.elems $ indexToData hrMin)
  where hrMax = maximumBy (comparing sizeOUT) [hr1, hr2]
        hrMin = minimumBy (comparing sizeOUT) [hr1, hr2]

-- --------------------------------------------------------------------------------
-- -- Example section
-- --------------------------------------------------------------------------------

exHyperRelation :: HyperRelation '[String, Int, Char]
exHyperRelation = HyperRelation ( IM.fromList [(1, (Identity "carlo" :<->:
                                                    Identity 25      :<->:
                                                    Identity 'n'     :<->: EndR))])
                                ( HM.fromList [("carlo" , IS.fromList [1])]
                            :<=>: HM.fromList [(25      , IS.fromList [1])]
                            :<=>: HM.fromList [('n'     , IS.fromList [1])]
                            :<=>: EndHR)

exHyperRelation2 :: HyperRelation '[String, Int, Char]
exHyperRelation2 = fromList [ ("carlo"  , 25, 'c')
                            , ("valerio", 21, 'v') ]

-- Sarebbe bello avere un combinatore generico per le iperrelazioni, ovvero data
-- una funzione per estrarre qualcosa dalla prima mappa e una serie di funzioni
-- per estrarre le cose dalla seconda, restituisce un risultato compiuto.

-- * Iniettivita'
-- Quello che ci manca da fare adesso e' inserire il combinatore per determinare
-- l'iniettivita' della mappa di cui ci stiamo occupando: di base, abbiamo bisogno
-- di un costruttore come:

-- #+BEGIN_EXAMPLE
-- Injective (Set Nat) (Set Nat)
-- #+END_EXAMPLE

-- Che viene poi astratto a livello dei tipi consentendoci di fare i check
-- opportuni.

-- Ecco come funzionano i check. Si controlla che non siano gia' presenti relazioni
-- che contengono gli elementi dell'insieme di arrivo specificati. Se questi
-- elementi esistono, allora si controlla che non ci sia qualcosa con gli esatti
-- elementi di partenza (dev'essere al piu' una a causa dell'iniettivita'). Se
-- esistono, si fallisce, con una modalita' appropriata. Se invece non esistono, si
-- include il nuovo legame.

-- Immagino che l'unica funzione che va veramente modificata una volta aggiunto
-- questo macchinario e' insertIN, perche' deve prendere come parametro anche il
-- set di constraint che gli vengono forniti dalla relazione.

-- L'idea sarebbe quella di usare RelationSideAt per essere in grado di guardare
-- che l'inserzione possa essere fatta.
