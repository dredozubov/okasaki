{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Chapter2.Exercise_2_3 where

import           UnbalancedSet.Common
import           UnbalancedSet.Lazy as L
import           UnbalancedSet.Strict as S

import           Control.Monad
import           Data.Coerce
import           Data.Proxy

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- Inserting an existing element into a binary search tree copies the
-- entire search path even though the copied nodes are indistinguishable
-- from the originals. Rewrite insert using exceptions(nope! - D.R.) to
-- avoid this copying. Establish only one handler per insertion rather than
-- one handler per iteration.

-- Maybe-based solution

-- lazy
newtype Ex_2_3 a = Ex_2_3 (Tree a)

uInsertMaybe :: Ord a => a -> Tree a -> Tree a
uInsertMaybe e t = case uInsertMaybe' e t of
  Just t' -> t'
  Nothing -> t
  where
    uInsertMaybe' :: Ord a => a -> Tree a -> Maybe (Tree a)
    uInsertMaybe' e L.E         = Just (L.T L.E e L.E)
    uInsertMaybe' e t@(L.T a n b)
      | e < n      = case uInsertMaybe' e a of
        Just res -> Just (L.T res n b)
        Nothing  -> Just t
      | e > n      = case uInsertMaybe' e b of
        Just res -> Just (L.T a n res)
        Nothing  -> Just t
      | otherwise  = Nothing

instance Ord e => UnbalancedSet Ex_2_3 e where
  uEmpty = Ex_2_3 L.E
  uMember = uMember . coerce
  uInsert e = coerce . uInsertMaybe e . coerce

-- strict
newtype Ex_2_3Strict a = Ex_2_3Strict (TreeStrict a)

uInsertMaybeStrict :: Ord a => a -> TreeStrict a -> TreeStrict a
uInsertMaybeStrict e t = case uInsertMaybeStrict' e t of
  Just t' -> t'
  Nothing -> t
  where
    uInsertMaybeStrict' :: Ord a => a -> TreeStrict a -> Maybe (TreeStrict a)
    uInsertMaybeStrict' e S.E         = Just (S.T S.E e S.E)
    uInsertMaybeStrict' e t@(S.T a n b)
      | e < n      = case uInsertMaybeStrict' e a of
        Just res -> Just (S.T res n b)
        Nothing  -> Just t
      | e > n      = case uInsertMaybeStrict' e b of
        Just res -> Just (S.T a n res)
        Nothing  -> Just t
      | otherwise  = Nothing

instance Ord e => UnbalancedSet Ex_2_3Strict e where
  uEmpty = Ex_2_3Strict S.E
  uMember = uMember . coerce
  uInsert e = coerce . uInsertMaybeStrict e . coerce

-- CPS solution

--lazy
newtype Ex_2_3_cps a = Ex_2_3_cps (Tree a)

uInsertCPS :: Ord a => a -> Tree a -> Tree a
uInsertCPS e t = uInsertCPS' e t id
  where
    uInsertCPS' :: Ord a => a -> Tree a -> ((Tree a -> r) -> r)
    uInsertCPS' e L.E         = \k -> k (L.T L.E e L.E)
    uInsertCPS' e t@(L.T a n b)
      | e < n      = \k -> k (L.T (uInsertCPS' e a id) n b)
      | e > n      = \k -> k (L.T a n (uInsertCPS' e b id))
      | otherwise  = \k -> k t

instance Ord e => UnbalancedSet Ex_2_3_cps e where
  uEmpty = Ex_2_3_cps L.E
  uMember = uMember . coerce
  uInsert e = coerce . uInsertCPS e . coerce

-- strict
newtype Ex_2_3_cpsStrict a = Ex_2_3_cpsStrict (TreeStrict a)

uInsertCPSStrict :: Ord a => a -> TreeStrict a -> TreeStrict a
uInsertCPSStrict e t = uInsertCPSStrict' e t id
  where
    uInsertCPSStrict' :: Ord a => a -> TreeStrict a -> ((TreeStrict a -> r) -> r)
    uInsertCPSStrict' e S.E         = \k -> k (S.T S.E e S.E)
    uInsertCPSStrict' e t@(S.T a n b)
      | e < n      = \k -> k (S.T (uInsertCPSStrict' e a id) n b)
      | e > n      = \k -> k (S.T a n (uInsertCPSStrict' e b id))
      | otherwise  = \k -> k t

instance Ord e => UnbalancedSet Ex_2_3_cpsStrict e where
  uEmpty = Ex_2_3_cpsStrict S.E
  uMember = uMember . coerce
  uInsert e = coerce . uInsertCPSStrict e . coerce

-- tests
tests2_3 :: Group
tests2_3 = Group "exercise 2.3 - no copy" $
  [ ("Maybe - lazy", prop_insert (Proxy @Ex_2_3))
  , ("Maybe - strict", prop_insert_strict (Proxy @Ex_2_3Strict))
  , ("Maybe - lazy", prop_insert (Proxy @Ex_2_3_cps))
  , ("Maybe - strict", prop_insert_strict (Proxy @Ex_2_3_cpsStrict))
  ]

main :: IO ()
main = void $ checkParallel tests2_3
