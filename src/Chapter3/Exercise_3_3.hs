module Chapter3.Exercise_3_3 where

import LeftistHeap

-- ex 3.3
-- Implement a function `fromList` of type `[t] -> LHeap t` that produces
-- a leftist heap from an unordered list of elements by first converting each
-- element into a singleton heap and then merging the heaps until only one heap
-- remains. Instead of merging the heaps in one right-to-left or left-to-right
-- pass using `foldr` or `foldl`, merge the heaps in `log n` passes, where
-- each pass merges adjacent pairs of heaps. Show that `fromList` takes only
-- O(n) time.
fromList :: [a] -> LHeap a
fromList = undefined
