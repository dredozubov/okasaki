module Exercise_2_3 where

import Chapter2
import Data.Coerce

newtype Ex_2_3 a = Ex_2_3 (Tree a)

-- Inserting an existing element into a binary search tree copies the
-- entire search path even though the copied nodes are indistinguishable
-- from the originals. Rewrite insert using exceptions(nope! - D.R.) to
-- avoid this copying. Establish only one handler per insertion rather than
-- one handler per iteration.
uInsertMaybe :: Ord a => a -> Tree a -> Tree a
uInsertMaybe e t = case uInsertMaybe' e t of
  Just t' -> t'
  Nothing -> t
  where
    uInsertMaybe' :: Ord a => a -> Tree a -> Maybe (Tree a)
    uInsertMaybe' e E         = Just (T E e E)
    uInsertMaybe' e t@(T a n b)
      | e < n      = Just (T (uInsert e a) n b)
      | e > n      = Just (T a n (uInsert e b))
      | otherwise  = Nothing

instance Ord e => UnbalancedSet Ex_2_3 e where
  uEmpty = Ex_2_3 E
  uMember = uMember . coerce
  uInsert e = coerce . uInsertMaybe e . coerce

newtype Ex_2_3_cps a = Ex_2_3_cps (Tree a)

uInsertCPS :: Ord a => a -> Tree a -> Tree a
uInsertCPS e t = uInsertCPS' e t id
  where
    uInsertCPS' :: Ord a => a -> Tree a -> ((Tree a -> r) -> r)
    uInsertCPS' e E         = \k -> k (T E e E)
    uInsertCPS' e t@(T a n b)
      | e < n      = \k -> k (T (uInsertCPS' e a id) n b)
      | e > n      = \k -> k (T a n (uInsertCPS' e b id))
      | otherwise  = \k -> k t

instance Ord e => UnbalancedSet Ex_2_3_cps e where
  uEmpty = Ex_2_3_cps E
  uMember = uMember . coerce
  uInsert e = coerce . uInsertCPS e . coerce
