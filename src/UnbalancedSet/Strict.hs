{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UnbalancedSet.Strict
  ( TreeStrict(..)
  , fromList
  , uMemberTreeStrict
  , uInsertTreeStrict
  , prop_insert_strict
  , prop_member_strict
  , tests
  , memberBenchmarkStrict
  , insertBenchmarkStrict
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
data TreeStrict e = E | T !(TreeStrict e) !e !(TreeStrict e)
  deriving (Show, Generic, Eq)

instance NFData a => NFData (TreeStrict a)

uMemberTreeStrict :: Ord a => a -> TreeStrict a -> Bool
uMemberTreeStrict m E = False
uMemberTreeStrict m (T a n b)
  | m == n  = True
  | m < n   = case a of
    E -> False
    t@(T _ _ _) -> uMemberTreeStrict m t
  | m > n   = case b of
    E -> False
    t@(T _ _ _) -> uMemberTreeStrict m t

uInsertTreeStrict :: Ord a => a -> TreeStrict a -> TreeStrict a
uInsertTreeStrict e E         = T E e E
uInsertTreeStrict e t@(T a n b)
  | e < n      = T (uInsertTreeStrict e a) n b
  | e > n      = T a n (uInsertTreeStrict e b)
  | otherwise  = t

instance Ord e => UnbalancedSet TreeStrict e where
  uEmpty = E
  uMember = uMemberTreeStrict
  uInsert = uInsertTreeStrict

-- helpers

toList :: TreeStrict a -> [a]
toList E = []
toList (T a n b) = toList a ++ n : toList b

fromList :: Ord a => [a] -> TreeStrict a
fromList = foldr uInsert E

-- May generate tree which is not BST
genTreeStrict :: Gen (TreeStrict Int)
genTreeStrict =
  Gen.recursive Gen.choice
    [ pure E ]
    [ Gen.subtermM2 genTreeStrict genTreeStrict (\a b -> do
      x <- Gen.int (Range.linear 0 10000)
      pure $ T a x b)
    ]

-- Generates a BST
genBST :: Gen (TreeStrict Int)
genBST =
  fromList <$> Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10000))

-- Tests

prop_member_strict :: forall t. (UnbalancedSet t Int, Coercible (TreeStrict Int) (t Int)) => Proxy t -> Property
prop_member_strict _ = withTests 5000 . property $ do
  t :: TreeStrict Int <- forAll genBST
  e <- forAll $ Gen.int (Range.linear 0 10000)
  uMember e (coerce t :: t Int) === elem e (toList t)

prop_insert_strict :: forall t. (UnbalancedSet t Int, Coercible (TreeStrict Int) (t Int), Coercible (t Int) (TreeStrict Int)) => Proxy t -> Property
prop_insert_strict _ = withTests 5000 . property $ do
  t :: TreeStrict Int <- forAll genBST
  e <- forAll $ Gen.int (Range.linear 0 10000)
  toList (coerce (uInsert e (coerce t :: t Int))) ===
    let l = toList t in if elem e l then l else insert e (toList t)

tests :: Group
tests = Group "Strict BST"
  [ ( "prop_member", prop_member_strict (Proxy @TreeStrict))
  , ("prop_insert", prop_insert_strict (Proxy @TreeStrict))
  ]

-- benchmarks
memberBenchmarkStrict
  :: forall t
  . (UnbalancedSet t Int, Coercible (TreeStrict Int) (t Int))
  => Int     -- ^ element to look up
  -> [TreeStrict Int] -- ^ average case values and trees
  -> [TreeStrict Int] -- ^ worst case values and trees
  -> Proxy t
  -> Benchmark
memberBenchmarkStrict e ats wts t = bgroup "strict"
  [ bench "average case - member" $ nf (uMember e . (coerce :: TreeStrict Int -> t Int) <$>) ats
  , bench "worst case - member" $ nf (uMember e . (coerce :: TreeStrict Int -> t Int) <$>) wts
  ]

insertBenchmarkStrict
  :: forall t
  . ( NFData (t Int)
    , Typeable t
    , UnbalancedSet t Int
    , Coercible (TreeStrict Int) (t Int)
    , Coercible (t Int) (TreeStrict Int) )
  => Int     -- ^ element to look up
  -> [TreeStrict Int] -- ^ average case values and trees
  -> [TreeStrict Int] -- ^ worst case values and trees
  -> Proxy t
  -> Benchmark
insertBenchmarkStrict e ats wts p = bgroup ("Strict " ++ show (typeRep p))
  [ bench "average case - insert" $ nf (uInsert e . (coerce :: TreeStrict Int -> t Int) <$>) ats
  , bench "worst case - insert" $ nf (uInsert offender . (coerce :: TreeStrict Int -> t Int) <$>) wts
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
      ats = force $!! fromList <$> l
      wts = force $!! fromList . sort <$> l
    defaultMain
      [ memberBenchmarkStrict e ats wts (Proxy @TreeStrict)
      , insertBenchmarkStrict e ats wts (Proxy @TreeStrict)
      ]
