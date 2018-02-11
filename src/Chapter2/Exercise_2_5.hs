module Chapter2.Exercise_2_5 where


import UnbalancedSet.Lazy

-- Sharing can also be useful within a single object, not just between objects.
-- For example, if two subtrees of a given node are identical, then they can be
-- represented by the same tree.

-- ex 2.5a
-- Using this idea, write a function `complete` of type `Elem * Int -> Tree`
-- where `complete (x,d)` creates a complete binary tree of depth `d` with `x`
-- stored in every node. (Of course, this function makes no sense for the set
-- abstraction, but it can be useful as an auxiliary function for other
-- abstractions, such as bags.) This function should run in O(d) time.
complete :: Int -> Int -> Tree Int
complete e 0 = E
complete e n = let s = complete e (n-1) in T s n s

-- ex 2.5b
-- Extend this function to create balanced trees of arbitrary size. These trees
-- will not always be complete binary trees, but should be as balanced as
-- possible: for any given node, the two subtrees should differ in size by at
-- most one. This function should run in O(log n) time. (Hint: use a helper
-- function `create2` that, given a size `m`, creates a pair of trees, one of
-- size `m` and one of size `m+1`.

-- create2 :: Enum a => a -> Int -> (Tree a, Tree a)
-- create2 e 0 = (E, T E e (succ e))
-- create2 e m =
--   let
--     ll = m - 1 `div` 2
--     lr = m - 1 - ll
--   in _


-- bst :: a -> Int -> Tree a
