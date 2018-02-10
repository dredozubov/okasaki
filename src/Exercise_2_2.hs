{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Exercise_2_2 where

import Chapter2

import           Control.Monad
import           Data.Coerce
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype Ex_2_2 a = Ex_2_2 (Tree a)

-- ex 2.2
-- rewrites uMember in a way that does maximum (d + 1) comparisons for depth d.
-- It does so by keeping the track of the tree element after doing a comparison,
-- but ignoring an equality check.
ex2_2 :: Ord e => e -> Tree e -> Bool
ex2_2 e E         = False
ex2_2 e (T a n b) =
  if e < n
  then ex2_2 e a
  else ex2_2' n b
  where
    ex2_2' x E         = e == x
    ex2_2' x (T a n b) = if e < n
      then ex2_2' x a
      else ex2_2' x b

instance Ord a => UnbalancedSet Ex_2_2 a where
  uEmpty = Ex_2_2 E
  uMember e (Ex_2_2 t) = ex2_2 e t
  uInsert e = coerce . uInsertTree e . coerce

-- ex 2.3

-- prop_member :: Property
-- prop_member =
--   property $ do
--     xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--     uMember xs === uMember (Ex_2_2 xs)

-- tests :: IO Bool
-- tests =
--   checkParallel $ Group "Exercise" [
--       ("prop_member", prop_member)
--     ]

-- main :: IO ()
-- main = do
--   void $ tests
