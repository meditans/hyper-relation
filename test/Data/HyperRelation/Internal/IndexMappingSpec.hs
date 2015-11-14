{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Data.HyperRelation.Internal.IndexMappingSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property.Monad

import qualified Data.HyperRelation.Internal.IndexMapping as IM
import Data.HyperRelation.Internal.IndexMapping (IndexMapping)
import Data.HashMap.Strict (HashMap)
import Data.Hashable

import Control.Applicative (liftA2)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The IndexMapping" $ do
    it "should work fine" $ do
      True `shouldBe` True
    it "should be obvious" $ do
      property $ \x xs -> head (x:xs) == (x::Int)
    it "should have positive size" $ do
      property $ satisfiesInvariant hasPositiveSize "Has positive size"

randomOperation :: Gen (String, IndexMapping String -> IndexMapping String)
randomOperation = oneof
                  [ liftA2 (\i s -> ("Inserting " ++ show i ++ " " ++ s, IM.insert i s)) arbitrary arbitrary
                  , fmap (\i -> ("Deleting" ++ show i, IM.deleteIndex i)) arbitrary
                  ]

newModel = IM.empty

hasPositiveSize im = IM.size im >= 0
hasBoundedSize  im = IM.size im <  5

satisfiesInvariant :: (IndexMapping String -> Bool) -> String -> Gen Property
satisfiesInvariant invariant msg = sized $ return . property . go newModel
  where
    go :: IndexMapping String -> Int -> PropM Bool
    go _     0    = return True
    go model size = do
      (description, operation) <- gen randomOperation
      logMessageLn ("Operation: " ++ description)
      let model' = operation model
      logMessageLn ("Model is now: " ++ show model')
      assert  msg  (invariant model')
      go model' (pred size)
