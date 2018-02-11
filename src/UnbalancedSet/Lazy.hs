{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UnbalancedSet.Lazy
  ( Tree(..)
  , fromList
  , uMemberTree
  , uInsertTree
  , prop_insert
  , prop_member
  , tests
  , memberBenchmark
  , insertBenchmark
  ) where

import           UnbalancedSet.Common

import           Control.DeepSeq
import           Control.Monad
import           Data.Coerce
import           Data.List
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics

import           Criterion.Main
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- 2.2 BSTs
data Tree e = E | T (Tree e) e (Tree e)
  deriving (Show, Generic, Eq)

instance NFData a => NFData (Tree a)

uMemberTree :: Ord a => a -> Tree a -> Bool
uMemberTree m E = False
uMemberTree m (T a n b)
  | m == n  = True
  | m < n   = case a of
    E -> False
    t@(T _ _ _) -> uMemberTree m t
  | m > n   = case b of
    E -> False
    t@(T _ _ _) -> uMemberTree m t

uInsertTree :: Ord a => a -> Tree a -> Tree a
uInsertTree e E         = T E e E
uInsertTree e t@(T a n b)
  | e < n      = T (uInsertTree e a) n b
  | e > n      = T a n (uInsertTree e b)
  | otherwise  = t

instance Ord e => UnbalancedSet Tree e where
  uEmpty = E
  uMember = uMemberTree
  uInsert = uInsertTree


-- helpers

toList :: Tree a -> [a]
toList E = []
toList (T a n b) = toList a ++ n : toList b

fromList :: Ord a => [a] -> Tree a
fromList = foldr uInsert E

-- May generate tree which is not BST
genTree :: Gen (Tree Int)
genTree =
  Gen.recursive Gen.choice
    [ pure E ]
    [ Gen.subtermM2 genTree genTree (\a b -> do
      x <- Gen.int (Range.linear 0 10000)
      pure $ T a x b)
    ]

-- generates a BST
genBST :: Gen (Tree Int)
genBST =
  fromList <$> Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10000))

-- tests

prop_member
  :: forall t
   . (UnbalancedSet t Int, Coercible (Tree Int) (t Int))
  => Proxy t
  -> Property
prop_member _ = withTests 5000 . property $ do
  t :: Tree Int <- forAll genBST
  e <- forAll $ Gen.int (Range.linear 0 10000)
  uMember e (coerce t :: t Int) === elem e (toList t)

prop_insert
  :: forall t
  . (UnbalancedSet t Int, Coercible (Tree Int) (t Int), Coercible (t Int) (Tree Int))
  => Proxy t
  -> Property
prop_insert _ = withTests 5000 . property $ do
  t :: Tree Int <- forAll genBST
  e <- forAll $ Gen.int (Range.linear 0 10000)
  toList (coerce (uInsert e (coerce t :: t Int))) ===
    let l = toList t in if elem e l then l else insert e (toList t)

tests :: Group
tests = Group "Lazy BST"
  [ ( "prop_member", prop_member (Proxy @Tree))
  , ("prop_insert", prop_insert (Proxy @Tree))
  ]

-- benchmarks
memberBenchmark
  :: forall t
  . (UnbalancedSet t Int, Coercible (Tree Int) (t Int))
  => Int     -- ^ element to look up
  -> [Tree Int] -- ^ average case values and trees
  -> [Tree Int] -- ^ worst case values and trees
  -> Proxy t
  -> Benchmark
memberBenchmark e ats wts t = bgroup "lazy"
  [ bench "average case - member" $ nf (uMember e . (coerce :: Tree Int -> t Int) <$>) ats
  , bench "worst case - member" $ nf (uMember e . (coerce :: Tree Int -> t Int) <$>) wts
  ]

insertBenchmark
  :: forall t
  . ( Typeable t
    , NFData (t Int)
    , UnbalancedSet t Int
    , Coercible (Tree Int) (t Int)
    , Coercible (t Int) (Tree Int) )
  => Int     -- ^ element to look up
  -> [Tree Int] -- ^ average case values and trees
  -> [Tree Int] -- ^ worst case values and trees
  -> Proxy t
  -> Benchmark
insertBenchmark e ats wts p = bgroup ("lazy " ++ show (typeRep p))
  [ bench "average case - insert" $ nf (uInsert e . (coerce :: Tree Int -> t Int) <$>) ats
  , bench "worst case - insert" $ nf (uInsert offender . (coerce :: Tree Int -> t Int) <$>) wts
  ]
  where
    offender :: Int
    offender  = maximum' (maximum <$> filter null $ toList . coerce <$> wts)
    maximum' :: [Int] -> Int
    maximum' xs = if null xs then 42 else maximum xs

main :: IO ()
main = do
  testResult <- checkParallel tests
  when testResult $ do
    e <- Gen.sample $ Gen.int (Range.linear 0 10000)
    l <- replicateM 1000 $ do
      Gen.sample $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10000))
    let
      ats = force $! fromList <$> l
      wts = force $! fromList . sort <$> l
    defaultMain
      [ memberBenchmark e ats wts (Proxy @Tree)
      , insertBenchmark e ats wts (Proxy @Tree)
      ]
