{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Chapter2.Exercise_2_4 where

import           UnbalancedSet.Common
import           UnbalancedSet.Lazy as L
import           UnbalancedSet.Strict as S

import           Control.Monad
import           Data.Coerce
import           Data.Proxy

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- ex 2.4
-- Combine the ideas of the previous two exercises to obtain a version of insert
-- that performs no unnecessary copying and uses no more than d+1 comparisons

-- lazy
newtype Ex_2_4 a = Ex_2_4 (Tree a)

betterInsert :: Ord a => a -> Tree a -> Tree a
betterInsert e L.E = L.T L.E e L.E
betterInsert e t'@(L.T _ x' _) = go x' t' t' id
  where
    go x s L.E = if e == x
      then \k -> k L.E
      else \k -> k (L.T L.E e L.E)
    go x s t@(L.T a n b) = if e < n
      then \k -> k (L.T (go x s a id) n b)
      else \k -> k (L.T a n (go n t b id))

instance Ord a => UnbalancedSet Ex_2_4 a where
  uEmpty = Ex_2_4 L.E
  uMember = uMember . coerce
  uInsert e = coerce . betterInsert e . coerce

-- strict
newtype Ex_2_4Strict a = Ex_2_4Strict (TreeStrict a)

betterInsertStrict :: Ord a => a -> TreeStrict a -> TreeStrict a
betterInsertStrict e S.E = S.T S.E e S.E
betterInsertStrict e t'@(S.T _ x' _) = go x' t' t' id
  where
    go x s S.E = if e == x
      then \k -> k S.E
      else \k -> k (S.T S.E e S.E)
    go x s t@(S.T a n b) = if e < n
      then \k -> k (S.T (go x s a id) n b)
      else \k -> k (S.T a n (go n t b id))

instance Ord a => UnbalancedSet Ex_2_4Strict a where
  uEmpty = Ex_2_4Strict S.E
  uMember = uMember . coerce
  uInsert e = coerce . betterInsertStrict e . coerce

tests2_4 :: Group
tests2_4 = Group "exercise 2.4 - d+1 comparisons + no-copying" $
  [ ("lazy", prop_insert (Proxy @Ex_2_4))
  , ("strict", prop_insert_strict (Proxy @Ex_2_4Strict))
  ]

main :: IO ()
main = do
  void $ checkParallel tests2_4
