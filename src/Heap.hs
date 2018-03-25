module Heap where

class Heap h a where
  hEmpty :: h a
  hIsEmpty :: h a -> Bool
  hInsert :: a -> h a -> h a
  hMerge :: h a -> h a -> h a
  hFindMin :: h a -> Maybe a
  hDeleteMin :: h a -> Maybe (h a)
