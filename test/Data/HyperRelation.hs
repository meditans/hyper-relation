{-# LANGUAGE DataKinds #-}
module Data.HyperRelationSpec where

import Data.HyperRelation

hyperRel :: HyperRelation '[Int, String, Int]
hyperRel  =  fromList  ([ (1,  "uno",     3)
                        , (2,  "due",     3)
                        , (3,  "tre",     3)
                        , (4,  "quattro", 7)
                        , (5,  "cinque",  6)
                        , (6,  "sei",     3)
                        , (7,  "sette",   5)
                        , (8,  "otto",    4)
                        , (9,  "nove",    4)
                        , (10, "dieci",   5)
                        , (11, "undici",  6)
                        , (12, "dodici",  6)
                        ] :: [(Int, String, Int)])
