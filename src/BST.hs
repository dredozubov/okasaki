-- | Binary search trees
module BST where

data Tree a = E | T !(Tree a) a !(Tree a) deriving (Show, Eq, Ord)

empty :: Tree a
empty = E

{-|
>>> insert 'f' E == T E 'f' E
True
-}
insert :: (Ord a) => a -> Tree a -> Tree a
insert x E         = T E x E
insert x (T a r b) = if x > r then insert x b else insert x a

{-|
>>> member 'f' E
False

>>> member 'f' (T E 'f' E)
True

>>> member 'b' (T (T E 'b' E) 'c' E)
True
-}
member :: (Ord a) => a -> Tree a -> Bool
member _ E         = False
member x (T a y b) = case compare x y of
  EQ -> True
  LT -> member x a
  GT -> member x b

{-|
ex 2.2
>>> member' 'f' E
False

>>> member' 'f' (T E 'f' E)
True

>>> member' 'c' (T (T E 'b' E) 'c' E)
True

>>> member' 'c' (T E 'c' (T E 'd' E))
True
-}
member' :: (Ord a) => a -> Tree a -> Bool
member' _ E = False
member' x t@(T _ y _) = go y t
  where
    go y E         = y == x
    go c (T a y b) = if x > y then go y b else go y a
