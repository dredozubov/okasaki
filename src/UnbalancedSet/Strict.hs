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
  , uMemberTreeStrict
  , uInsertTreeStrict
  , prop_insert_strict
  , prop_member_strict
  , tests
  ) where


import UnbalancedSet.Common

import Control.DeepSeq
import Control.Monad
import Data.Coerce
import Data.List
import Data.Proxy
import GHC.Generics

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

genBST :: Gen (TreeStrict Int)
genBST =
  fromList <$> Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10000))

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

main :: IO ()
main = do
  void $ checkParallel tests
