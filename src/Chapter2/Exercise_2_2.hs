{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Chapter2.Exercise_2_2 where

import           UnbalancedSet.Common
import           UnbalancedSet.Lazy as L
import           UnbalancedSet.Strict as S

import           Control.Monad
import           Data.Coerce
import           Data.Proxy

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype Ex_2_2 a = Ex_2_2 (Tree a)
newtype Ex_2_2Strict a = Ex_2_2Strict (TreeStrict a)

-- ex 2.2
-- rewrites uMember in a way that does maximum (d + 1) comparisons for depth d.
-- It does so by keeping the track of the tree element after doing a comparison,
-- but ignoring an equality check.
ex2_2 :: Ord e => e -> Tree e -> Bool
ex2_2 e L.E         = False
ex2_2 e t@(L.T _ x _) = ex2_2' x t
  where
    ex2_2' n L.E         = n == e
    ex2_2' n (L.T a x b) = if e < x
      then ex2_2' n a
      else ex2_2' x b

ex2_2strict :: Ord e => e -> TreeStrict e -> Bool
ex2_2strict e S.E         = False
ex2_2strict e t@(S.T _ x _) = ex2_2' x t
  where
    ex2_2' n S.E         = n == e
    ex2_2' n (S.T a x b) = if e < x
      then ex2_2' n a
      else ex2_2' x b

instance Ord a => UnbalancedSet Ex_2_2 a where
  uEmpty = Ex_2_2 L.E
  uMember e (Ex_2_2 t) = ex2_2 e t
  uInsert e = coerce . uInsertTree e . coerce

instance Ord a => UnbalancedSet Ex_2_2Strict a where
  uEmpty = Ex_2_2Strict S.E
  uMember e (Ex_2_2Strict t) = ex2_2strict e t
  uInsert e = coerce . uInsertTreeStrict e . coerce

tests2_2 :: Group
tests2_2 = Group "exercise 2.2 - d+1 comparisons" $
  [ ("lazy", prop_member (Proxy @Ex_2_2))
  , ("strict", prop_member_strict (Proxy @Ex_2_2Strict))
  ]

main :: IO ()
main = do
  void $ checkParallel tests2_2
