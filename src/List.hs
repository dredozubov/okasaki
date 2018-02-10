module List where

{-|
>>> suffixes [1..3]
[[1,2,3],[2,3],[3],[]]

>>> suffixes []
[[]]
-}
suffixes :: [a] -> [[a]]
suffixes []       = [[]]
suffixes l@(_:tl) = l : suffixes tl
