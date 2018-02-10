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
  , uMemberTree
  , uInsertTree
  , prop_insert
  , prop_member
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

genBST :: Gen (Tree Int)
genBST =
  fromList <$> Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10000))

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
tests = Group "Strict BST"
  [ ( "prop_member", prop_member (Proxy @Tree))
  , ("prop_insert", prop_insert (Proxy @Tree))
  ]

main :: IO ()
main = do
  void $ checkParallel tests
