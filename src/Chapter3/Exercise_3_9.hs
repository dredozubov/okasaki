{-# LANGUAGE OverloadedStrings #-}

module Chapter3.Exercise_3_9 where

import RedBlackSet hiding (tests)

import Control.Monad
import Data.List

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- ex 3.9
-- Write a function `fromOrdList` of type `[e] -> t e` that converts a sorted
-- list with no duplicates into a red-black tree. Your function should run in
-- O(n) time.
fromOrdList :: [e] -> Tree e
fromOrdList [] = E
fromOrdList l = T B leftSubTree sh rightSubTree
  where
    (first, sh:second) = splitAt (length l `div` 2) l
    go :: Tree e -> e -> Tree e
    go E e             = T R E e E
    go (T c a x E) e = T c a x (T (invert c) E e E)
    go t@(T c _ _ _) e = T (invert c) t e E
    leftSubTree = foldl' go E first
    rightSubTree = foldl' go E second

invert :: Color -> Color
invert R = B
invert B = R

prop_fromOrdList_balanced :: Property
prop_fromOrdList_balanced = withTests 5000 . property $ do
  i <- forAll $ Gen.integral (Range.linear 0 10000)
  isBalanced (fromOrdList [1..i]) === True

tests :: Group
tests = Group "ex 3.9"
  [ ("prop_fromOrdList_balanced", prop_fromOrdList_balanced) ]

main :: IO ()
main = void $ checkParallel tests
