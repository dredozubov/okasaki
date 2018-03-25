module Chapter3.Exercise_3_4 where

import LeftistHeap

-- ex 3.4
-- (a) Prove that right spine of a weight-biased leftist heap contains at most
-- `log(n+1)` elements.
-- (b) Modify the implementation in of `instance Heap LHeap a` to obtain
-- weight-biased leftist heaps.
-- (c) Currently, `merge` operates in two passes: a top-down pass consisting of
-- calls to `merge`, and a bottom-up pass consisting of calls to the helper
-- function `makeT`. Modify `merge` for weight-biased leftist heaps to operate
-- in a single, top-down pass.
-- (d) What advantages would the top-down version of `merge` have in a lazy
-- environment? In a concurrent environment?
