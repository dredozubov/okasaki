{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RedBlackSet where

import           Control.DeepSeq
import           Control.Monad
import           Data.Coerce
import           Data.List as L
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)

import           Criterion.Main
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


data Color = R | B deriving (Show, Eq, Generic)

instance NFData Color

data Tree a = E | T !Color !(Tree a) !a !(Tree a) deriving (Show, Eq, Generic)

instance NFData a => NFData (Tree a)

class RedBlackSet t e where
  rbEmpty :: t e
  rbMember :: Ord e => e -> t e -> Bool
  rbBalance :: Ord e => Color -> t e -> e -> t e -> t e
  rbInsert :: Ord e => e -> t e -> t e

instance RedBlackSet Tree e where
  rbEmpty = E
  rbMember _ E = False
  rbMember e (T _ a x b) =
    case e `compare` x of
      EQ -> True
      LT -> rbMember e a
      GT -> rbMember e b
  rbBalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
  rbBalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
  rbBalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
  rbBalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
  rbBalance c a x b                     = T c a x b
  rbInsert e = blacken . ins
    where
      ins E             = (T R E e E)
      ins t@(T c a x b) = case e `compare` x of
        EQ -> t
        LT -> rbBalance c (ins a) x b
        GT -> rbBalance c a x (ins b)

blacken :: Tree e -> Tree e
blacken E           = E
blacken (T _ a x b) = T B a x b

isBalanced :: Tree a -> Bool
isBalanced E                                     = True
isBalanced (T R E _ E)                           = True
isBalanced (T B E _ E)                           = True
isBalanced (T c a@(T ca _ _ _) _ b@(T cb _ _ _)) =
  let cIsRed = c == R in
  not (cIsRed && ca == R) &&
  not (cIsRed && cb == R) &&
  isBalanced a && isBalanced b
isBalanced (T c a@(T ca _ _ _) _ _)              = c /= ca && isBalanced a
isBalanced (T c _ _ b@(T cb _ _ _))              = c /= cb&& isBalanced b

-- helpers
toList :: Tree a -> [a]
toList E = []
toList (T _ a n b) = toList a ++ n : toList b

fromList :: Ord a => [a] -> Tree a
fromList = foldr rbInsert E

-- May generate tree which is not BST
genTree :: Gen (Tree Int)
genTree =
  Gen.recursive Gen.choice
    [ pure E ]
    [ Gen.subtermM2 genTree genTree (\a b -> do
      x <- Gen.int (Range.linear 0 10000)
      c <- Gen.choice [pure R, pure B]
      pure $ T c a x b)
    ]

-- Generates a RBT
genRBT :: Gen (Tree Int)
genRBT =
  fromList <$> Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10000))

-- Tests

prop_rbMember_strict :: forall t. (RedBlackSet t Int, Coercible (Tree Int) (t Int)) => Proxy t -> Property
prop_rbMember_strict _ = withTests 5000 . property $ do
  t :: Tree Int <- forAll genRBT
  e <- forAll $ Gen.int (Range.linear 0 10000)
  rbMember e (coerce t :: t Int) === elem e (toList t)

prop_rbInsert_strict :: forall t. (RedBlackSet t Int, Coercible (Tree Int) (t Int), Coercible (t Int) (Tree Int)) => Proxy t -> Property
prop_rbInsert_strict _ = withTests 5000 . property $ do
  t :: Tree Int <- forAll genRBT
  e <- forAll $ Gen.int (Range.linear 0 10000)
  toList (coerce (rbInsert e (coerce t :: t Int))) ===
    let l = toList t in if elem e l then l else insert e (toList t)

prop_rbInsert_balanced :: Property
prop_rbInsert_balanced = withTests 5000 . property $ do
  t :: Tree Int <- forAll genRBT
  isBalanced t === True

tests :: Group
tests = Group "Strict RedBlackTree"
  [ ("prop_rbInsert_balanced", prop_rbInsert_balanced)
  , ("prop_rbInsert", prop_rbInsert_strict (Proxy @Tree))
  , ( "prop_rbMember", prop_rbMember_strict (Proxy @Tree))
  ]

-- benchmarks
rbMemberBenchmarkStrict
  :: forall t
  . (RedBlackSet t Int, Coercible (Tree Int) (t Int))
  => Int     -- ^ element to look up
  -> [Tree Int] -- ^ average case values and trees
  -> [Tree Int] -- ^ worst case values and trees
  -> Proxy t
  -> Benchmark
rbMemberBenchmarkStrict e ats wts _ = bgroup "strict"
  [ bench "average case - rbMember" $ nf (rbMember e . (coerce :: Tree Int -> t Int) <$>) ats
  , bench "worst case - rbMember" $ nf (rbMember e . (coerce :: Tree Int -> t Int) <$>) wts
  ]

rbInsertBenchmarkStrict
  :: forall t
  . ( NFData (t Int)
    , Typeable t
    , RedBlackSet t Int
    , Coercible (Tree Int) (t Int)
    , Coercible (t Int) (Tree Int) )
  => Int     -- ^ element to look up
  -> [Tree Int] -- ^ average case values and trees
  -> [Tree Int] -- ^ worst case values and trees
  -> Proxy t
  -> Benchmark
rbInsertBenchmarkStrict e ats wts p = bgroup ("Strict " ++ show (typeRep p))
  [ bench "average case - rbInsert" $ nf (rbInsert e . (coerce :: Tree Int -> t Int) <$>) ats
  , bench "worst case - rbInsert" $ nf (rbInsert offender . (coerce :: Tree Int -> t Int) <$>) wts
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
      [ rbInsertBenchmarkStrict e ats wts (Proxy @Tree)
      , rbMemberBenchmarkStrict e ats wts (Proxy @Tree)
      ]
