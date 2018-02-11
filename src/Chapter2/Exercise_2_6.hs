{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Chapter2.Exercise_2_6 where

import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Ord

-- ex 2.6
-- Adapt the `UnbalancedSet` functor to support finite maps rather than sets.
class FiniteMap m k a where
  empty :: m k a
  bind :: k -> a -> m k a -> m k a
  lookup :: k -> m k a -> Maybe a

data Map k a = E | T (Map k a) k a (Map k a)

instance Ord k => FiniteMap Map k a where
  empty = E
  bind k v E           = T E k v E
  bind k v (T a n _ b)
    | k < n     = bind k v a
    | k > n     = bind k v b
    | otherwise = T a k v b
  lookup k E = Nothing
  lookup k (T a n v b) = lookup' (n,v) (T a n v b)
    where
      lookup' (x,v) E = if k == x then Just v else Nothing
      lookup' p@(x,_) (T a n v b) = if x < n
        then lookup' p a
        else lookup' (n,v) b
