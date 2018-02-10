module Chapter2 where

import Control.DeepSeq
import GHC.Generics

class Ord e => UnbalancedSet t e where
  uEmpty :: t e
  uMember :: e -> t e -> Bool
  uInsert :: e -> t e -> t e

-- 2.2 BSTs
data Tree e = E | T !(Tree e) !e !(Tree e)
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
