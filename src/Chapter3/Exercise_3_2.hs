module Chapter3.Exercise_3_2 where

import LeftistHeap

-- ex 3.2
-- Define `insert` directly rather than via a call to `merge`
-- insert :: Ord a => a -> LHeap a -> LHeap a
-- insert e E = T 1 E e E
-- insert e (T r a x b) = if e <= x
--   then
--     let st = makeT x a b
--     in T (hRank st + 1) st e E
--   else _
